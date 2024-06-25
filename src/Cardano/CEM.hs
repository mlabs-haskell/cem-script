{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.CEM where

import PlutusTx.Prelude
import Prelude (Show)
import Prelude qualified

import Data.Data (Proxy)
import Data.Map qualified as Map
import Data.Proxy (Proxy (..))
import GHC.Records (HasField (..))
import GHC.TypeLits (KnownSymbol, Symbol)

-- Plutus imports
import PlutusLedgerApi.V1.Address (Address, pubKeyHashAddress)
import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V2 (ScriptContext, ToData (..), Value)
import PlutusTx qualified
import PlutusTx.Show.TH (deriveShow)

import Data.Singletons.TH

-- Project imports
import Cardano.CEM.Stages (SingleStage, Stages (..))
import Data.Spine (HasFieldNum, HasSpine (..))
import GHC.OverloadedLabels

singletons
  [d|
    data CVar = CParams | CState | CTransition | CComp | CContext
    |]

---

-- | This is different ways to specify address
data AddressSpec
  = ByAddress Address
  | ByPubKey PubKeyHash
  | BySameScript
  deriving stock (Show, Prelude.Eq)

{-# INLINEABLE addressSpecToAddress #-}
addressSpecToAddress :: Address -> AddressSpec -> Address
addressSpecToAddress ownAddress addressSpec = case addressSpec of
  ByAddress address -> address
  ByPubKey pubKey -> pubKeyHashAddress pubKey
  BySameScript -> ownAddress

data TxFanFilter script = MkTxFanFilter
  { address :: AddressSpec
  , rest :: TxFanFilter' script
  }
  deriving stock (Show, Prelude.Eq)

data TxFanFilter' script
  = Anything
  | -- | To be used via `bySameCem`
    UnsafeBySameCEM BuiltinData
  | ByDatum BuiltinData
  deriving stock (Show, Prelude.Eq)

{-# INLINEABLE bySameCEM #-}

-- | Constraint enforcing state of script mentioning this constraint
bySameCEM ::
  (ToData (State script), CEMScript script) =>
  State script ->
  TxFanFilter' script
bySameCEM = UnsafeBySameCEM . toBuiltinData

-- TODO: use natural numbers
data Quantor = Exist Integer | SumValueEq Value
  deriving stock (Show)

data TxFanKind = In | InRef | Out
  deriving stock (Prelude.Eq, Prelude.Show)

data TxFanConstraint script = MkTxFanC
  { txFanCKind :: TxFanKind
  , txFanCFilter :: TxFanFilter script
  , txFanCQuantor :: Quantor
  }
  deriving stock (Show)

--

data TxFanFilterNew f script
  = UserAddress (f PubKeyHash)
  | SameScript (f (State script))
  | OtherScript -- CEMScript CEMParams

deriving stock instance
  ( Show (State script)
  , forall x. (Show x) => Show (f x)
  ) =>
  (Show (TxFanFilterNew f script))

data TxFanConstraint' (f :: * -> *) script
  = TxFan
      { kind :: TxFanKind
      , cFilter :: TxFanFilterNew f script
      , value :: f Value
      }
  | AdditionalSigner (f PubKeyHash)
  | -- TODO
    Embed (f (TxFanConstraint' f script))
  | Noop

deriving stock instance
  ( Show (State script)
  , forall x. (Show x) => Show (f x)
  ) =>
  (Show (TxFanConstraint' f script))

type family CVarType (cvar :: CVar) script where
  CVarType CParams script = Params script
  CVarType CState script = State script
  CVarType CTransition script = Transition script
  CVarType CComp script = TransitionComp script
  CVarType CContext script = ScriptContext

type HasFieldX (a :: Symbol) b c = (HasFieldNum a b c)

data ConstraintDSL script value where
  Ask ::
    forall (var :: CVar) datatype script.
    ( SingI var
    , datatype Prelude.~ (CVarType var script)
    ) =>
    Proxy var ->
    ConstraintDSL script datatype
  Pure :: (ToData value') => value' -> ConstraintDSL script value'
  -- TODO: move to constraint?
  Check ::
    TxFanConstraint' (ConstraintDSL script) script ->
    ConstraintDSL script value'
  Error :: BuiltinString -> ConstraintDSL script value
  If ::
    ConstraintDSL script Bool ->
    ConstraintDSL script value ->
    ConstraintDSL script value ->
    ConstraintDSL script value
  IsOnChain :: ConstraintDSL script Bool
  -- TODO
  GetField ::
    forall (label :: Symbol) x script value.
    (HasFieldX label x value) =>
    ConstraintDSL script x ->
    Proxy label ->
    ConstraintDSL script value
  OfSpine ::
    forall script datatype spine.
    (spine Prelude.~ Spine datatype, Prelude.Enum spine) =>
    spine ->
    [RecordSetter (ConstraintDSL script) datatype] ->
    ConstraintDSL script datatype
  -- Primitives
  Eq ::
    forall x script.
    (Eq x) =>
    ConstraintDSL script x ->
    ConstraintDSL script x ->
    ConstraintDSL script Bool
  Ge ::
    -- | Or equal
    Bool ->
    ConstraintDSL script Integer ->
    ConstraintDSL script Integer ->
    ConstraintDSL script Bool
  AdaValue :: ConstraintDSL script Integer -> ConstraintDSL script Value

minLovelace = AdaValue $ Pure 3_000_000

(@==) :: (Eq x) => ConstraintDSL script x -> ConstraintDSL script x -> ConstraintDSL script Bool
(@==) = Eq
(@>=) = Ge True
(@>) = Ge False
(@<=) = flip (@>=)
(@<) = flip (@>)

checkOnchainOnly ::
  TxFanConstraint' (ConstraintDSL script) script ->
  TxFanConstraint' (ConstraintDSL script) script
checkOnchainOnly c = Embed (If IsOnChain (Check Noop) (Check c))

data MyLabel (label :: Symbol) = MkMyLabel

instance (KnownSymbol s1, s1 ~ s2) => IsLabel (s1 :: Symbol) (MyLabel s2) where
  fromLabel :: MyLabel s2
  fromLabel = MkMyLabel

data RecordSetter f datatype where
  (::=) ::
    forall f (label :: Symbol) datatype value.
    (HasFieldX label datatype value) =>
    MyLabel label ->
    f value ->
    RecordSetter f datatype

instance
  (HasFieldX label datatype value) =>
  HasField label (ConstraintDSL script datatype) (ConstraintDSL script value)
  where
  getField recordDsl =
    GetField @label @datatype @script @value recordDsl Proxy

instance Show (ConstraintDSL x y) where
  show _ = "ConstraintDSL" -- TODO

askC ::
  forall (var :: CVar) script.
  (SingI var) =>
  ConstraintDSL script (CVarType var script)
askC = Ask @var @_ @script (Proxy :: Proxy var)

ctxParams = askC @CParams
ctxTransition = askC @CTransition
ctxState = askC @CState
ctxComp = askC @CComp

ask ::
  forall (var :: CVar) (label :: Symbol) script value.
  (HasFieldX label (CVarType var script) value, SingI var) =>
  ConstraintDSL script value
ask = GetField @label (Ask @var @_ @script (Proxy :: Proxy var)) Proxy

-- Main API

-- FIXME: move IsData here (now it breaks Plutus compilation)
type DefaultConstraints datatype =
  ( Prelude.Eq datatype
  , Prelude.Show datatype
  )

{- | All associated types for `CEMScript`
They are separated to simplify TH deriving
-}
class CEMScriptTypes script where
  -- \| `Params` is immutable part of script Datum,
  -- \| it should be used to encode all
  type Params script = params | params -> script

  -- | `Stage` is datatype encoding all `Interval`s specified by script.
  -- | `Stage` logic is encoded by separate `Stages` type class.
  -- | It have separate `StageParams` datatype,
  -- | which is stored immutable in script Datum as well.
  type Stage script

  type Stage script = SingleStage

  -- | `State` is changing part of script Datum.
  -- | It is in
  type State script = params | params -> script

  -- | Transitions for deterministic CEM-machine
  type Transition script = transition | transition -> script

  type TransitionComp script
  type TransitionComp script = Void

class
  ( HasSpine (Transition script)
  , HasSpine (State script)
  , HasSpine (Params script)
  , HasSpine (TransitionComp script)
  , Prelude.Enum (Spine (Transition script))
  , Prelude.Bounded (Spine (Transition script))
  , Stages (Stage script) (StageParams (Stage script))
  , DefaultConstraints (Stage script)
  , DefaultConstraints (Transition script)
  , DefaultConstraints (State script)
  , DefaultConstraints (Params script)
  , DefaultConstraints (StageParams (Stage script))
  , CEMScriptTypes script
  ) =>
  CEMScript script
  where
  -- | Each kind of Transition has statically associated Stage
  -- from/to `State`s spines
  transitionStage ::
    Proxy script ->
    Map.Map
      (Spine (Transition script))
      ( Stage script
      , Maybe (Spine (State script))
      , Maybe (Spine (State script))
      )

  transitionComp ::
    Maybe
      ( Params script ->
        Maybe (State script) ->
        Transition script ->
        TransitionComp script
      )
  transitionComp = Nothing

  transitionSpec' ::
    Map.Map
      (Spine (Transition script))
      [TxFanConstraint' (ConstraintDSL script) script]
  transitionSpec' = Map.empty

  -- This functions define domain logic
  transitionSpec ::
    Params script ->
    Maybe (State script) ->
    Transition script ->
    Either BuiltinString (TransitionSpec script)

data TransitionSpec script = MkTransitionSpec
  { constraints :: [TxFanConstraint script]
  , -- List of additional signers (in addition to one required by TxIns)
    signers :: [PubKeyHash]
  }
  deriving stock (Show)

instance ToData (TransitionSpec script) where
  toBuiltinData _ = toBuiltinData ()

-- | List of all signing keys required for transition spec
getAllSpecSigners :: TransitionSpec script -> [PubKeyHash]
getAllSpecSigners spec = signers spec ++ txInPKHs
  where
    txInPKHs = mapMaybe getPubKey $ filter ((Prelude.== In) . txFanCKind) $ constraints spec
    getPubKey c = case address (txFanCFilter c) of
      ByPubKey key -> Just key
      _ -> Nothing

{- | Static part of CEMScript datum.
Datatype is actually used only by off-chain code due to Plutus limitations.
-}
data CEMParams script = MkCEMParams
  { scriptParams :: Params script
  , stagesParams :: StageParams (Stage script)
  }

deriving stock instance (CEMScript script) => (Show (CEMParams script))
deriving stock instance (CEMScript script) => (Prelude.Eq (CEMParams script))

-- FIXME: documentation
type CEMScriptDatum script =
  (StageParams (Stage script), Params script, State script)

-- TH deriving done at end of file for GHC staging reasons

deriveShow ''TxFanKind
deriveShow ''TxFanFilter'
