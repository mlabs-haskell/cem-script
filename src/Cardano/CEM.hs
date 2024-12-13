{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- FIXME: move all lib functions (`LiftPlutarch`s) to another module
-- FIXME:
{-
Module reorganization:

- CEM - main class type and friends
- Constraint a.k.a DSL
- Plutarch stuff

-}

module Cardano.CEM where

import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Singletons.TH
import Data.Spine (HasPlutusSpine, HasSpine (..), derivePlutusSpine, spineFieldsNum)
import Data.Text (Text)
import GHC.OverloadedLabels (IsLabel (..))
import GHC.Records (HasField (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Plutarch (Config (..), (#))
import Plutarch.Builtin (PIsData)
import Plutarch.Evaluate (evalTerm)
import Plutarch.Extras
import Plutarch.LedgerApi (KeyGuarantees (..))
import Plutarch.LedgerApi.Value
import Plutarch.Lift (PUnsafeLiftDecl (..), pconstant, plift)
import Plutarch.Prelude (
  ClosedTerm,
  PLift,
  PPartialOrd (..),
  PShow,
  Term,
  pnot,
  (#&&),
  (:-->),
 )
import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V2 (ToData (..), Value)
import PlutusLedgerApi.V2.Contexts (TxInfo)
import PlutusTx qualified
import PlutusTx.Builtins qualified as PlutusTx
import Unsafe.Coerce (unsafeCoerce)
import Prelude

-- Types of variables a CEM Script operates over, can be accessed and/or
-- updated from CEM constraints, see 'TxConstraint'.
data CVar
  = -- | CEM parameters, see 'CEMScriptTypes'
    CParams
  | -- | CEM states, see 'CEMScriptTypes'
    CState
  | -- | CEM transitions, see 'CEMScriptTypes'
    CTransition
  | -- | Optional custom computation, see 'transitionComp'
    CComp
  | -- | Plutus transaction context FIXME: how do we use it? Only debugging?
    CTxInfo
  deriving stock (Show)

genSingletons [''CVar]

-- | Calculates a type for a variable type for a particular script.
type family CVarType (cvar :: CVar) script where
  CVarType CParams script = Params script
  CVarType CState script = State script
  CVarType CTransition script = Transition script
  CVarType CComp script = TransitionComp script
  CVarType CTxInfo script = TxInfo

{- | During the initial stage of compilation DSL Values
move from unresolved state where they are represented
by `ConstraintDSL script value` into `value`.
-}
type family DSLValue (resolved :: Bool) script value where
  DSLValue False script value = ConstraintDSL script value
  DSLValue True _script value = value

{- | DSL Patterns should be handled during that compilation stage
and should not be used in resolved constraints.
-}
type family DSLPattern (resolved :: Bool) script value where
  DSLPattern False script value = ConstraintDSL script value
  DSLPattern True _ value = Void

{- | FIXME: Everyone is confused by term "fan" here.
TxO?
Output?
TxInOut
?
-}
data TxFanKind
  = -- | tx inputs
    In
  | -- | tx reference inputs
    InRef
  | -- | tx outputs
    Out
  deriving stock (Eq, Show)

-- TODO: TxInOutConstraint?
-- FIXME: Wait, why Filter? These are constraints, no?
-- FIXME: Shall we add `Noop` or use Maybe (TxFanFilter resolved script) in TxConstraint?
data TxFanFilter (resolved :: Bool) script
  = UserAddress (DSLValue resolved script PubKeyHash)
  | -- FIXME: should have spine been specified known statically
    SameScript (DSLValue resolved script (State script))

deriving stock instance (CEMScript script) => (Show (TxFanFilter True script))
deriving stock instance (Show (TxFanFilter False script))

-- | Constraints are the root elements of the DSL.

-- TODO: rename, why Tx? Transition? Just Constraint?
data TxConstraint (resolved :: Bool) script
  = TxFan
      { kind :: TxFanKind
      , cFilter :: TxFanFilter resolved script
      , value :: DSLValue resolved script Value
      }
  | MainSignerCoinSelect
      { user :: DSLValue resolved script PubKeyHash
      , inValue :: DSLValue resolved script Value
      , outValue :: DSLValue resolved script Value
      }
  | MainSignerNoValue (DSLValue resolved script PubKeyHash)
  | Error Text
  | If
      -- | Condition
      (DSLPattern resolved script Bool)
      -- | Then block
      (TxConstraint resolved script)
      -- | Else block
      (TxConstraint resolved script)
  | forall sop.
    (HasPlutusSpine sop) =>
    MatchBySpine
      -- | Value being matched by its Spine
      (DSLPattern resolved script sop)
      -- | Case switch
      (Map.Map (Spine sop) (TxConstraint resolved script))
  | -- | Dummy noop constraint
    Noop

deriving stock instance (CEMScript script) => (Show (TxConstraint True script))
deriving stock instance (Show (TxConstraint False script))

type HasFieldPlutus (a :: Symbol) d v =
  ( HasField a d v
  , HasPlutusSpine d
  , KnownSymbol a
  , ToData v
  )

type PlutarchData x = (PShow x, PLift x, PIsData x)

-- -----------------------------------------------------------------------------
-- DSL
-- -----------------------------------------------------------------------------

-- | DSL to express constraints
data ConstraintDSL script value where
  -- | Get access to context, see `CVarType` for available datatypes.
  Ask ::
    forall (var :: CVar) datatype script.
    ( SingI var
    , datatype ~ CVarType var script
    ) =>
    Proxy var ->
    ConstraintDSL script datatype
  -- | Lifts a Plutus value into DSL
  -- TODO: rename to Lift/Send?
  Pure ::
    (Show value', ToData value') =>
    value' ->
    ConstraintDSL script value'
  -- | Allows to skip checks conditionally, usually for on-chain.
  IsOnChain :: ConstraintDSL script Bool
  -- FIXME: should have Spine typechecked on DSL compilation,
  -- see `MatchBySpine`
  GetField ::
    forall (label :: Symbol) sop script value.
    (HasFieldPlutus label sop value) =>
    ConstraintDSL script sop ->
    Proxy label ->
    ConstraintDSL script value
  UnsafeOfSpine ::
    forall script datatype spine.
    ( spine ~ Spine datatype
    , HasPlutusSpine datatype
    ) =>
    Spine datatype ->
    [RecordSetter (ConstraintDSL script) datatype] ->
    ConstraintDSL script datatype
  -- FIXME: On-chain compilation bounds `UnsafeUpdateOfSpine` to tuple datum?
  -- Used with In
  UnsafeUpdateOfSpine ::
    forall script datatype spine.
    ( spine ~ Spine datatype
    , HasPlutusSpine datatype
    , PlutusTx.FromData datatype
    ) =>
    ConstraintDSL script datatype ->
    Spine datatype ->
    [RecordSetter (ConstraintDSL script) datatype] ->
    ConstraintDSL script datatype
  -- Primitives
  -- FIXME: learn Anything semantics?
  Anything :: ConstraintDSL script x
  Eq ::
    forall x script.
    (Eq x) =>
    ConstraintDSL script x ->
    ConstraintDSL script x ->
    ConstraintDSL script Bool
  LiftPlutarch ::
    forall px py script.
    (PlutarchData px, PlutarchData py) =>
    (ClosedTerm (px :--> py)) ->
    ConstraintDSL script (PLifted px) ->
    ConstraintDSL script (PLifted py)
  LiftPlutarch2 ::
    forall px1 px2 py script.
    (PlutarchData px1, PlutarchData px2, PlutarchData py) =>
    (forall s. Term s px1 -> Term s px2 -> Term s py) ->
    ConstraintDSL script (PLifted px1) ->
    ConstraintDSL script (PLifted px2) ->
    ConstraintDSL script (PLifted py)

-- TODO: use some pretty printer lib
instance Show (ConstraintDSL x y) where
  show dsl = case dsl of
    (Ask @cvar Proxy) ->
      "Ask " <> drop 1 (show (fromSing $ sing @cvar))
    (GetField valueDsl proxyLabel) ->
      show valueDsl <> "." <> symbolVal proxyLabel
    Eq x y -> show x <> " @== " <> show y
    -- FIXME: add user annotations
    LiftPlutarch _ x -> "somePlutarchCode (" <> show x <> ")"
    LiftPlutarch2 _ x y ->
      "somePlutarchCode (" <> show x <> ") (" <> show y <> ")"
    IsOnChain -> "IsOnChain"
    Anything -> "Anything"
    Pure x -> "Pure (" <> show x <> ")"
    UnsafeOfSpine spine setters ->
      "OfSpine " <> show spine <> show setters
    UnsafeUpdateOfSpine spine _ _ -> "UnsafeUpdateOfSpine " <> show spine

-- -----------------------------------------------------------------------------
-- Datatypes and instances for working with records, used in

data RecordSetter f datatype where
  (::=) ::
    forall (label :: Symbol) datatype value f.
    (HasFieldPlutus label datatype value) =>
    RecordLabel label ->
    f value ->
    RecordSetter f datatype

instance (forall x. Show (f x)) => Show (RecordSetter f datatype) where
  show ((::=) @label _label value) =
    symbolVal (Proxy @label) <> " ::= " <> show value

data RecordLabel (label :: Symbol) = MkRecordLabel

-- | This instance allow the use of OverloadedLabels
instance
  (KnownSymbol s1, s1 ~ s2) =>
  IsLabel (s1 :: Symbol) (RecordLabel s2)
  where
  fromLabel :: RecordLabel s2
  fromLabel = MkRecordLabel

instance
  (HasFieldPlutus label datatype value) =>
  HasField label (ConstraintDSL script datatype) (ConstraintDSL script value)
  where
  getField recordDsl =
    GetField @label @datatype @script @value recordDsl Proxy

-- -----------------------------------------------------------------------------
-- Helpers to be used in actual definitions

-- General Ask

askC ::
  forall (var :: CVar) script.
  (SingI var) =>
  ConstraintDSL script (CVarType var script)
askC = Ask @var @_ @script (Proxy :: Proxy var)

-- Specific Asks TODO: rename

ctxParams :: ConstraintDSL script (Params script)
ctxTransition :: ConstraintDSL script (Transition script)
ctxParams = askC @CParams
ctxTransition = askC @CTransition
ctxState :: ConstraintDSL script (State script)
ctxState = askC @CState
ctxComp :: ConstraintDSL script (TransitionComp script)
ctxComp = askC @CComp

-- Pure

lift ::
  (Show value', ToData value') =>
  value' ->
  ConstraintDSL script value'
lift = Pure

cMkAdaOnlyValue ::
  ConstraintDSL script Integer -> ConstraintDSL script Value
cMkAdaOnlyValue = LiftPlutarch pMkAdaOnlyValue

cEmptyValue :: ConstraintDSL script Value
cEmptyValue = cMkAdaOnlyValue $ lift 0

cMinLovelace :: ConstraintDSL script Value
cMinLovelace = cMkAdaOnlyValue $ lift 3_000_000

-- TODO: These both are updates
cOfSpine ::
  (HasPlutusSpine datatype) =>
  Spine datatype ->
  [RecordSetter (ConstraintDSL script) datatype] ->
  ConstraintDSL script datatype
-- FIXME: should it be ordered?
cOfSpine spine setters =
  if toInteger (length setters) == toInteger (spineFieldsNum spine)
    then UnsafeOfSpine spine setters
    else
      error $
        "OfSpine got less setters when number of fields ("
          <> show (spineFieldsNum spine)
          <> ")"

nullarySpine :: (HasPlutusSpine datatype) => Spine datatype -> ConstraintDSL script datatype
nullarySpine spine = cOfSpine spine []

cUpdateOfSpine ::
  (HasPlutusSpine datatype) =>
  ConstraintDSL script datatype ->
  Spine datatype ->
  [RecordSetter (ConstraintDSL script) datatype] ->
  ConstraintDSL script datatype
cUpdateOfSpine = UnsafeUpdateOfSpine

cUpdateOfSpine' ::
  (HasPlutusSpine datatype) =>
  ConstraintDSL script datatype ->
  Spine datatype ->
  ConstraintDSL script datatype
cUpdateOfSpine' orig spine = UnsafeUpdateOfSpine orig spine []

(@==) ::
  (Eq x) => ConstraintDSL script x -> ConstraintDSL script x -> ConstraintDSL script Bool
(@==) = Eq

(@<=) ::
  forall px script.
  (PlutarchData px, PPartialOrd px) =>
  ConstraintDSL script (PLifted px) ->
  ConstraintDSL script (PLifted px) ->
  ConstraintDSL script Bool
(@<=) = LiftPlutarch2 (#<=)

(@<) ::
  forall px script.
  (PlutarchData px, PPartialOrd px) =>
  ConstraintDSL script (PLifted px) ->
  ConstraintDSL script (PLifted px) ->
  ConstraintDSL script Bool
(@<) = LiftPlutarch2 (#<)

(@>=) ::
  forall px script.
  (PlutarchData px, PPartialOrd px) =>
  ConstraintDSL script (PLifted px) ->
  ConstraintDSL script (PLifted px) ->
  ConstraintDSL script Bool
(@>=) = flip (@<=)

(@>) ::
  forall px script.
  (PlutarchData px, PPartialOrd px) =>
  ConstraintDSL script (PLifted px) ->
  ConstraintDSL script (PLifted px) ->
  ConstraintDSL script Bool
(@>) = flip (@<)

(@<>) ::
  ConstraintDSL script Value ->
  ConstraintDSL script Value ->
  ConstraintDSL script Value
(@<>) =
  LiftPlutarch2 @(PValue 'Unsorted 'NonZero) @_ @_ merge
  where
    merge x y =
      pforgetSorted $
        (<>)
          (passertSorted # x)
          (passertSorted # y)

infixr 3 @&&

(@&&) ::
  ConstraintDSL script Bool ->
  ConstraintDSL script Bool ->
  ConstraintDSL script Bool
(@&&) = LiftPlutarch2 (#&&)

cNot :: ConstraintDSL script Bool -> ConstraintDSL script Bool
cNot = LiftPlutarch pnot

-- TxConstraint utils

-- | Check constraint only in the offchain
offchainOnly :: TxConstraint False script -> TxConstraint False script
offchainOnly c = If IsOnChain Noop c

byFlagError ::
  ConstraintDSL script Bool -> Text -> TxConstraint False script
byFlagError flag message = If flag (Error message) Noop

-- -----------------------------------------------------------------------------
-- Constraints resolving
-- -----------------------------------------------------------------------------

-- TODO: add note on datums and transitions
compileConstraint ::
  forall script.
  (CEMScript script) =>
  CEMScriptDatum script ->
  Transition script ->
  TxConstraint False script ->
  Either String (TxConstraint True script)
compileConstraint datum transition c = case c of
  If condDsl thenConstr elseConstr -> do
    value <- compileDslRecur condDsl
    if value
      then recur thenConstr
      else recur elseConstr
  MatchBySpine value caseSwitch ->
    recur . (caseSwitch Map.!) . getSpine =<< compileDslRecur value
  MainSignerNoValue signerDsl ->
    MainSignerNoValue <$> compileDslRecur signerDsl
  MainSignerCoinSelect pkhDsl inValueDsl outValueDsl ->
    MainSignerCoinSelect
      <$> compileDslRecur pkhDsl
      <*> compileDslRecur inValueDsl
      <*> compileDslRecur outValueDsl
  TxFan kind fanFilter valueDsl ->
    TxFan kind <$> compileFanFilter fanFilter <*> compileDslRecur valueDsl
  Noop -> Right Noop
  -- XXX: changing resolved type param of Error
  e@(Error {}) -> Right $ unsafeCoerce e
  where
    compileDslRecur :: ConstraintDSL script x -> Either String x
    compileDslRecur = compileDsl @script datum transition
    recur = compileConstraint @script datum transition
    compileFanFilter fanFilter = case fanFilter of
      UserAddress dsl -> UserAddress <$> compileDslRecur dsl
      SameScript stateDsl -> SameScript <$> compileDslRecur stateDsl

compileDsl ::
  forall script x.
  (CEMScript script) =>
  CEMScriptDatum script ->
  Transition script ->
  ConstraintDSL script x ->
  Either String x
compileDsl datum@(params, state) transition dsl = case dsl of
  Pure x -> Right x
  Ask @cvar @_ @dt Proxy ->
    case sing @cvar of
      SCParams -> Right params
      SCState -> Right state
      SCTransition -> Right transition
      SCComp -> case transitionComp @script of
        Just go -> Right $ go params state transition
        Nothing -> error "Unreachable"
      SCTxInfo -> raiseOnchainErrorMessage ("TxInfo reference" :: String)
  IsOnChain -> Right False
  GetField @label @datatype @_ @value recordDsl _ -> do
    recordValue <- recur recordDsl
    Right $ getField @label @datatype @value recordValue
  Eq @v xDsl yDsl -> (==) <$> (recur @v) xDsl <*> (recur @v) yDsl
  UnsafeOfSpine spine recs -> do
    rs <- mapM compileRecordSetter recs
    Right $
      fromJust . PlutusTx.fromData . PlutusTx.builtinDataToData $
        PlutusTx.mkConstr
          (toInteger $ fromEnum spine)
          rs
    where
      compileRecordSetter (_ ::= valueDsl) = do
        value <- recur valueDsl
        Right $ PlutusTx.toBuiltinData value
  UnsafeUpdateOfSpine valueDsl _spine setters -> do
    case setters of
      [] -> recur valueDsl
      _ -> error "FIXME: not implemented"
  LiftPlutarch pterm argDsl -> do
    arg <- recur argDsl
    case evalTerm NoTracing $ pterm # pconstant arg of
      Right (Right resultTerm, _, _) -> Right $ plift resultTerm
      Right (Left message, _, _) ->
        Left $ "Unreachable: plutach running error " <> show message
      Left message -> Left $ "Unreachable: plutach running error " <> show message
  LiftPlutarch2 pterm arg1Dsl arg2Dsl -> do
    arg1 <- recur arg1Dsl
    arg2 <- recur arg2Dsl
    case evalTerm NoTracing $ pterm (pconstant arg1) (pconstant arg2) of
      Right (Right resultTerm, _, _) -> Right $ plift resultTerm
      Right (Left message, _, _) ->
        Left $ "Unreachable: plutach running error " <> show message
      Left message -> Left $ "Unreachable: plutach running error " <> show message
  Anything -> raiseOnchainErrorMessage dsl
  where
    raiseOnchainErrorMessage :: (Show a) => a -> Either String x
    raiseOnchainErrorMessage x =
      Left $
        "On-chain only feature was reached while off-chain constraints compilation "
          <> "(should be guarded to only triggered onchain): "
          <> show x
    recur :: ConstraintDSL script x1 -> Either String x1
    recur = compileDsl @script datum transition

-- -----------------------------------------------------------------------------
-- Main CEM Script API
-- -----------------------------------------------------------------------------

-- | TODO:
type CEMScriptSpec resolved script =
  ( Map.Map
      (Spine (Transition script))
      [TxConstraint resolved script]
  )

-- | Type class to define a CEM Script. Works together with 'CEMScriptTypes'.
class
  ( HasPlutusSpine (Transition script)
  , HasSpine (State script)
  , HasSpine (Params script)
  , DefaultConstraints (Transition script)
  , DefaultConstraints (State script)
  , DefaultConstraints (Params script)
  , DefaultConstraints (TransitionComp script)
  , CEMScriptTypes script
  ) =>
  CEMScript script
  where
  -- | This map defines constraints for each transition via DSL
  -- FIXME: name
  perTransitionScriptSpec :: CEMScriptSpec False script

  -- | Optional Plutus script to calculate things, for the cases when
  -- CEM constrainsts and/or inlining Plutarch functions are not
  -- expresisble enough.
  transitionComp ::
    Maybe
      ( Params script ->
        State script ->
        Transition script ->
        TransitionComp script
      )
  {-# INLINEABLE transitionComp #-}
  transitionComp = Nothing

  compilationConfig :: CompilationConfig

  -- TODO: remove

  -- | This is the map of all possible machine 'Transition's.
  -- This statically associates every 'Transition' with
  -- a 'Stage' through source/target 'State's.
  transitionStage ::
    Proxy script ->
    Map.Map
      (Spine (Transition script))
      ( Maybe (Spine (State script)) -- source 'State'
      , Maybe (Spine (State script)) -- target 'State'
      )
  transitionStage _ = Map.empty

type DefaultConstraints datatype =
  ( Eq datatype
  , Show datatype
  , PlutusTx.UnsafeFromData datatype
  , PlutusTx.FromData datatype
  , PlutusTx.ToData datatype
  )

{- | All associated types for a 'CEMScript'.
They are separated to simplify TH deriving.
-}
class CEMScriptTypes script where
  -- | The immutable part of script datum
  type Params script = params | params -> script

  -- | The changable part of script datum
  type State script = params | params -> script

  -- | Available transitions for deterministic CEM-machine
  type Transition script = transition | transition -> script

  -- | See 'transitionComp'
  type TransitionComp script

  -- FIXME: any reason to have NoTransitionComp?
  type TransitionComp script = Void -- NoTransitionComp

newtype CompilationConfig = MkCompilationConfig
  { errorCodesPrefix :: String
  }

type CEMScriptDatum script = (Params script, State script)

-- data NoTransitionComp = MkNoTransitionComp
--   deriving stock (Eq, Show)

-- TH deriving done at end of file for GHC staging reasons

-- derivePlutusSpine ''NoTransitionComp
