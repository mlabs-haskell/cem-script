{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- FIXME: move all lib functions (`LiftPlutarch`s) to another module
module Cardano.CEM where

import Prelude

import Data.Map qualified as Map
import Data.Maybe (fromJust)
import GHC.OverloadedLabels (IsLabel (..))
import GHC.Records (HasField (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Unsafe.Coerce (unsafeCoerce)

-- Plutus imports
import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V2 (ToData (..), Value)
import PlutusTx qualified
import PlutusTx.Builtins qualified as PlutusTx

import Data.Singletons.TH
import Data.Text (Text)
import Plutarch (Config (..), (#))
import Plutarch.Builtin (PIsData)
import Plutarch.Evaluate (evalTerm)
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
import PlutusLedgerApi.V2.Contexts (TxInfo)

-- Project imports
import Data.Spine (HasPlutusSpine, HasSpine (..), derivePlutusSpine)
import Plutarch.Extras

data CVar = CParams | CState | CTransition | CComp | CTxInfo
  deriving stock (Show)

genSingletons [''CVar]

type family DSLValue (resolved :: Bool) script value where
  DSLValue False script value = ConstraintDSL script value
  DSLValue True _ value = value

-- | This value should not used on resolved constraint
type family DSLPattern (resolved :: Bool) script value where
  DSLPattern False script value = ConstraintDSL script value
  DSLPattern True _ value = Void

data TxFanKind = In | InRef | Out
  deriving stock (Prelude.Eq, Prelude.Show)

data TxFanFilterNew (resolved :: Bool) script
  = UserAddress (DSLValue resolved script PubKeyHash)
  | -- FIXME: should have spine been specified known statically
    SameScript (DSLValue resolved script (State script))

deriving stock instance (CEMScript script) => (Show (TxFanFilterNew True script))
deriving stock instance (Show (TxFanFilterNew False script))

data TxConstraint (resolved :: Bool) script
  = TxFan
      { kind :: TxFanKind
      , cFilter :: TxFanFilterNew resolved script
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
      -- FIXME: might use function instead, will bring `_` syntax,
      -- reusing matched var and probably implicitly type-checking spine
      -- by saving it to such var DSL value
      (Map.Map (Spine sop) (TxConstraint resolved script))
  | Noop

deriving stock instance (CEMScript script) => (Show (TxConstraint True script))
deriving stock instance (Show (TxConstraint False script))

type family CVarType (cvar :: CVar) script where
  CVarType CParams script = Params script
  CVarType CState script = State script
  CVarType CTransition script = Transition script
  CVarType CComp script = TransitionComp script
  CVarType CTxInfo script = TxInfo

type HasFieldPlutus (a :: Symbol) d v =
  ( HasField a d v
  , HasPlutusSpine d
  , KnownSymbol a
  , ToData v
  )

type PlutarchData x = (PShow x, PLift x, PIsData x)

data ConstraintDSL script value where
  Ask ::
    forall (var :: CVar) datatype script.
    ( SingI var
    , datatype Prelude.~ CVarType var script
    ) =>
    Proxy var ->
    ConstraintDSL script datatype
  Pure ::
    (Show value', ToData value') =>
    value' ->
    ConstraintDSL script value'
  IsOnChain :: ConstraintDSL script Bool
  -- FIXME: should have Spine typechecked on DSL compilation,
  -- see `MatchBySpine`
  GetField ::
    forall (label :: Symbol) x script value.
    (HasFieldPlutus label x value) =>
    ConstraintDSL script x ->
    Proxy label ->
    ConstraintDSL script value
  UnsafeOfSpine ::
    forall script datatype spine.
    ( spine Prelude.~ Spine datatype
    , HasPlutusSpine datatype
    ) =>
    Spine datatype ->
    [RecordSetter (ConstraintDSL script) datatype] ->
    ConstraintDSL script datatype
  UnsafeUpdateOfSpine ::
    forall script datatype spine.
    ( spine Prelude.~ Spine datatype
    , HasPlutusSpine datatype
    , PlutusTx.FromData datatype
    ) =>
    ConstraintDSL script datatype ->
    Spine datatype ->
    [RecordSetter (ConstraintDSL script) datatype] ->
    ConstraintDSL script datatype
  -- Primitives
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
    (forall s. Term s (px1) -> Term s (px2) -> Term s (py)) ->
    ConstraintDSL script (PLifted px1) ->
    ConstraintDSL script (PLifted px2) ->
    ConstraintDSL script (PLifted py)

cOfSpine ::
  (HasPlutusSpine datatype) =>
  Spine datatype ->
  [RecordSetter (ConstraintDSL script) datatype] ->
  ConstraintDSL script datatype
cOfSpine = UnsafeOfSpine

-- TODO
cUpdateOfSpine ::
  (HasPlutusSpine datatype) =>
  ConstraintDSL script datatype ->
  Spine datatype ->
  [RecordSetter (ConstraintDSL script) datatype] ->
  ConstraintDSL script datatype
cUpdateOfSpine = UnsafeUpdateOfSpine

-- FIXUP
instance Prelude.Show (ConstraintDSL x y) where
  show dsl = case dsl of
    (Ask @cvar Proxy) ->
      "Ask " <> drop 1 (show (fromSing $ sing @cvar))
    (GetField valueDsl proxyLabel) ->
      Prelude.show valueDsl <> "." <> symbolVal proxyLabel
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
        Nothing -> Prelude.error "Unreachable"
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
          (toInteger $ Prelude.fromEnum spine)
          rs
    where
      compileRecordSetter (_ ::= valueDsl) = do
        value <- recur valueDsl
        Right $ PlutusTx.toBuiltinData value
  UnsafeUpdateOfSpine valueDsl _spine _recs -> do
    recur valueDsl -- TODO
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

cMkAdaOnlyValue ::
  ConstraintDSL script Integer -> ConstraintDSL script Value
cMkAdaOnlyValue = LiftPlutarch pMkAdaOnlyValue

cEmptyValue :: ConstraintDSL script Value
cEmptyValue = cMkAdaOnlyValue $ Pure 0

cMinLovelace :: ConstraintDSL script Value
cMinLovelace = cMkAdaOnlyValue $ Pure 3_000_000

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

offchainOnly :: TxConstraint False script -> TxConstraint False script
offchainOnly c = If IsOnChain Noop c

byFlagError ::
  ConstraintDSL script Bool -> Text -> TxConstraint False script
byFlagError flag message = If flag (Error message) Noop

data RecordLabel (label :: Symbol) = MkRecordLabel

instance
  (KnownSymbol s1, s1 ~ s2) =>
  IsLabel (s1 :: Symbol) (RecordLabel s2)
  where
  fromLabel :: RecordLabel s2
  fromLabel = MkRecordLabel

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

instance
  (HasFieldPlutus label datatype value) =>
  HasField label (ConstraintDSL script datatype) (ConstraintDSL script value)
  where
  getField recordDsl =
    GetField @label @datatype @script @value recordDsl Proxy

askC ::
  forall (var :: CVar) script.
  (SingI var) =>
  ConstraintDSL script (CVarType var script)
askC = Ask @var @_ @script (Proxy :: Proxy var)

ctxParams :: ConstraintDSL script (Params script)
ctxTransition :: ConstraintDSL script (Transition script)
ctxParams = askC @CParams
ctxTransition = askC @CTransition
ctxState :: ConstraintDSL script (State script)
ctxState = askC @CState
ctxComp :: ConstraintDSL script (TransitionComp script)
ctxComp = askC @CComp

-- Main API

data NoTransitionComp = MkNoTransitionComp
  deriving stock (Prelude.Eq, Prelude.Show)

type DefaultConstraints datatype =
  ( Prelude.Eq datatype
  , Prelude.Show datatype
  , PlutusTx.UnsafeFromData datatype
  , PlutusTx.FromData datatype
  , PlutusTx.ToData datatype
  )

{- | All associated types for `CEMScript`
They are separated to simplify TH deriving
-}
class CEMScriptTypes script where
  -- \| `Params` is immutable part of script Datum
  type Params script = params | params -> script

  -- | `State` is changing part of script Datum.
  -- | It is in
  type State script = params | params -> script

  -- | Transitions for deterministic CEM-machine
  type Transition script = transition | transition -> script

  -- | See `transitionComp`
  type TransitionComp script

  type TransitionComp script = NoTransitionComp

type CEMScriptSpec resolved script =
  ( Map.Map
      (Spine (Transition script))
      [TxConstraint resolved script]
  )

data CompilationConfig = MkCompilationConfig
  { errorCodesPrefix :: String
  }

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
  -- | Plutus script to calculate things,
  -- if DSL and inlining Plutarch functions are not expresisble enough
  transitionComp ::
    Maybe
      ( Params script ->
        State script ->
        Transition script ->
        TransitionComp script
      )
  {-# INLINEABLE transitionComp #-}
  transitionComp = Nothing

  -- | This map defines constraints for each transition via DSL
  perTransitionScriptSpec :: CEMScriptSpec False script

  compilationConfig :: CompilationConfig

-- FIXME: No need to use type synonym anymore (was needed due to Plutus)
type CEMScriptDatum script = (Params script, State script)

-- TH deriving done at end of file for GHC staging reasons

derivePlutusSpine ''NoTransitionComp
