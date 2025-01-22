{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fewer imports" #-}

module Cardano.CEM.DSL (
  -- * Main CEM Script API
  CEMScriptSpec,
  CEMScript (..),
  CEMScriptTypes (..),
  CompilationConfig (..),
  CEMScriptDatum,

  -- * Constraints
  TxConstraint (..),
  Utxo (..),
  UtxoKind (..),
  SameScriptArg (..),
  getMainSigner,
  getMbMainSigner,

  -- * DSL
  ConstraintDSL (..),
  DSLPattern,
  DSLValue,
  RecordSetter (..),
  SCVar (..),
  CParamsSym0,
  CStateSym0,
  CTransitionSym0,
  CCompSym0,
  CVar (..),
  CVarType,
  RecordLabel (..),
  PlutarchData,
) where

import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Singletons.TH
import Data.Spine (HasPlutusSpine, HasSpine (..))
import Data.Text (Text)
import GHC.OverloadedLabels (IsLabel (..))
import GHC.Records (HasField (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Plutarch.Builtin (PIsData)
import Plutarch.Lift (PUnsafeLiftDecl (..))
import Plutarch.Prelude (
  ClosedTerm,
  PLift,
  PShow,
  Term,
  (:-->),
 )
import PlutusLedgerApi.V1 (PubKeyHash)
import PlutusLedgerApi.V2 (ToData (..), Value)
import PlutusTx qualified
import Prelude

-- -----------------------------------------------------------------------------
-- Constraints DSL
-- -----------------------------------------------------------------------------

-- Types of variables the DSL language operates over, can be accessed and/or
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
  deriving stock (Show)

genSingletons [''CVar]

-- | Calculates a type for a variable type for a particular script.
type family CVarType (cvar :: CVar) script where
  CVarType CParams script = Params script
  CVarType CState script = State script
  CVarType CTransition script = Transition script
  CVarType CComp script = TransitionComp script

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
  Pure ::
    (Show value', ToData value') =>
    value' ->
    ConstraintDSL script value'
  -- | Allows to skip checks conditionally, usually for on-chain.
  IsOnChain :: ConstraintDSL script Bool
  GetField ::
    forall (label :: Symbol) sop script value.
    (HasFieldPlutus label sop value) =>
    ConstraintDSL script sop ->
    Proxy label ->
    ConstraintDSL script value
  -- | Builds a datatype value from the spine and field setters.
  -- Used in Utxo Out spec. Only specified fields are compared.
  UnsafeOfSpine ::
    forall script datatype spine.
    ( spine ~ Spine datatype
    , HasPlutusSpine datatype
    ) =>
    Spine datatype ->
    [RecordSetter (ConstraintDSL script) datatype] ->
    ConstraintDSL script datatype
  -- | More like Haskell `rec {field = value}` syntax.
  -- Used in Utxo In constraints.
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
    -- TODO: add user annotations
    LiftPlutarch _ x -> "somePlutarchCode (" <> show x <> ")"
    LiftPlutarch2 _ x y ->
      "somePlutarchCode (" <> show x <> ") (" <> show y <> ")"
    IsOnChain -> "IsOnChain"
    Anything -> "Anything"
    Pure x -> "Pure (" <> show x <> ")"
    UnsafeOfSpine spine setters ->
      "OfSpine " <> show spine <> show setters
    UnsafeUpdateOfSpine spine _ _ -> "UnsafeUpdateOfSpine " <> show spine

type HasFieldPlutus (a :: Symbol) d v =
  ( HasField a d v
  , HasPlutusSpine d
  , KnownSymbol a
  , ToData v
  )

type PlutarchData x = (PShow x, PLift x, PIsData x)

-- -----------------------------------------------------------------------------
-- Datatypes and instances for working with records
-- -----------------------------------------------------------------------------

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
-- Constraints, which are the main elements to build up a CEM Script.
-- -----------------------------------------------------------------------------

-- | Constraints are root elements of the DSL.
data TxConstraint (resolved :: Bool) script
  = Utxo
      { kind :: UtxoKind
      , spec :: Utxo resolved script
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

{- | Every transition involves some inputs and outputs.
We use term Utxo to refer to both, including refernce inputs.
-}
data Utxo (resolved :: Bool) script
  = -- | U utxo held by a user
    UserAddress (DSLValue resolved script PubKeyHash)
  | -- | Own utxo held by the same CEM script
    SameScript (SameScriptArg resolved script) -- (DSLValue resolved script (State script))

deriving stock instance (CEMScript script) => (Show (Utxo True script))
deriving stock instance (Show (Utxo False script))

-- | TODO: this wrapper is not needed
data SameScriptArg (resolved :: Bool) script where
  MkSameScriptArg ::
    DSLValue resolved script (State script) ->
    SameScriptArg resolved script

deriving stock instance (CEMScript script) => (Show (SameScriptArg True script))
deriving stock instance (Show (SameScriptArg False script))

data UtxoKind
  = -- | trantision inputs
    In
  | -- | reference inputs used in the transition
    InRef
  | -- | transition outputs
    Out
  deriving stock (Eq, Show)

getMainSigner :: [TxConstraint True script] -> PubKeyHash
getMainSigner cs = case mapMaybe f cs of
  [pkh] -> pkh
  _ ->
    error
      "Transition should have exactly one MainSigner* constraint"
  where
    f (MainSignerNoValue pkh) = Just pkh
    f (MainSignerCoinSelect pkh _ _) = Just pkh
    f _ = Nothing

getMbMainSigner :: [TxConstraint True script] -> Maybe PubKeyHash
getMbMainSigner cs = case mapMaybe f cs of
  [] -> Nothing
  [pkh] -> Just pkh
  _ ->
    error
      "Transition should have exactly one MainSigner* constraint"
  where
    f (MainSignerNoValue pkh) = Just pkh
    f (MainSignerCoinSelect pkh _ _) = Just pkh
    f _ = Nothing

-- -----------------------------------------------------------------------------
-- Main CEM Script API
-- -----------------------------------------------------------------------------

{- | CEM Script specification is a map of possible transitions with corresponding
set of constraints. See "Data.Spine" module to get an idea what 'Spine' is.
-}
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
  -- | The crux part - a map that defines constraints for each transition via DSL.
  transitionSpec :: CEMScriptSpec False script

  -- | Optional Plutus script to calculate a value of type
  -- (TransitionComp script), which can be used in the cases
  -- when CEM constraints and/or inlining Plutarch functions
  -- are not expressive enough.
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

  -- | By default it's not set.
  type TransitionComp script = Void

-- | Options used for compiling.
newtype CompilationConfig = MkCompilationConfig
  { errorCodesPrefix :: String
  }

-- | This is what CEM Scripts' datum look like.
type CEMScriptDatum script = (Params script, State script)
