{-# OPTIONS_GHC -Wno-orphans #-}

-- | Generic utils for using `quickcheck-dynamic`
module Cardano.CEM.Testing.StateMachine where

import Prelude

import Control.Monad (void)
import Control.Monad.Trans (MonadIO (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Data (Typeable)
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set

import PlutusLedgerApi.V1 (PubKeyHash)
import PlutusTx.IsData (FromData (..))

import Cardano.Api (PaymentKey, SigningKey, Value)

import Clb (ClbT)
import Test.QuickCheck
import Test.QuickCheck.DynamicLogic (DynLogicModel)
import Test.QuickCheck.Gen qualified as Gen
import Test.QuickCheck.Monadic (monadic)
import Test.QuickCheck.StateModel (
  Actions,
  Any (..),
  Generic,
  HasVariables (..),
  Realized,
  RunModel (..),
  StateModel (..),
  runActions,
 )
import Text.Show.Pretty (ppShow)

import Cardano.CEM (CEMParams (..))
import Cardano.CEM hiding (scriptParams)
import Cardano.CEM.Monads (MonadSubmitTx)
import Cardano.CEM.Monads.CLB
import Cardano.CEM.OffChain
import Cardano.CEM.OnChain (CEMScriptCompiled)
import Cardano.Extras
import Data.Spine (HasSpine (..))

data ScriptStateParams a = MkScriptStateParams
  { actors :: [SigningKey PaymentKey]
  , cemParams :: CEMParams a
  }
  deriving stock (Generic)

params :: ScriptStateParams script -> Params script
params = scriptParams . cemParams

deriving stock instance (Eq (CEMParams a)) => Eq (ScriptStateParams a)
deriving stock instance (Show (CEMParams a)) => Show (ScriptStateParams a)

data ScriptState a
  = Void
  | ActorsKnown [SigningKey PaymentKey]
  | ScriptState
      { dappParams :: ScriptStateParams a
      , state :: Maybe (State a)
      , involvedActors :: Set.Set PubKeyHash
      , finished :: Bool
      }
  deriving stock (Generic)

deriving stock instance
  (Eq (State a), Eq (CEMParams a)) => Eq (ScriptState a)
deriving stock instance
  (Show (State a), Show (CEMParams a)) => Show (ScriptState a)

instance HasVariables (ScriptState a) where
  getAllVariables _ = Set.empty

instance {-# OVERLAPS #-} HasVariables (Action (ScriptState script) a) where
  getAllVariables _ = Set.empty

class
  ( CEMScriptCompiled script
  , Show (Transition script)
  , Show (State script)
  , Show (CEMParams script)
  , Eq (State script)
  , Eq (CEMParams script)
  , Eq (Transition script)
  ) =>
  CEMScriptArbitrary script
  where
  arbitraryCEMParams :: [SigningKey PaymentKey] -> Gen (CEMParams script)
  arbitraryTransition ::
    ScriptStateParams script -> Maybe (State script) -> Gen (Transition script)

instance (CEMScriptArbitrary script) => StateModel (ScriptState script) where
  data Action (ScriptState script) output where
    SetupActors :: [SigningKey PaymentKey] -> Action (ScriptState script) ()
    SetupCEMParams :: CEMParams script -> Action (ScriptState script) ()
    ScriptTransition ::
      Transition script -> Action (ScriptState script) ()
    deriving stock (Typeable)

  type Error (ScriptState script) = String

  initialState = Void

  actionName (ScriptTransition transition) = head . words . show $ transition
  actionName SetupActors {} = "SetupActors"
  actionName SetupCEMParams {} = "SetupCEMParams"

  arbitraryAction _varCtx modelState = case modelState of
    Void {} -> Gen.oneof [] -- SetupActors should be done manually
    ActorsKnown actors ->
      Some . SetupCEMParams <$> arbitraryCEMParams actors
    ScriptState {dappParams, state} ->
      Some . ScriptTransition <$> arbitraryTransition dappParams state

  precondition Void (SetupActors {}) = True
  precondition (ActorsKnown {}) (SetupCEMParams {}) = True
  precondition
    (ScriptState {dappParams, state, finished})
    (ScriptTransition transition) =
      case transitionSpec @script (params dappParams) state transition of
        Right _ -> not finished
        Left _ -> False
  -- Unreachable
  precondition _ _ = False

  nextState Void (SetupActors actors) _var = ActorsKnown actors
  nextState (ActorsKnown actors) (SetupCEMParams cemParams) _var =
    ScriptState
      { dappParams = MkScriptStateParams {actors, cemParams}
      , state = Nothing
      , involvedActors = Set.empty
      , finished = False
      }
  nextState
    as@ScriptState {dappParams, state}
    (ScriptTransition transition)
    _var =
      case transitionSpec (params dappParams) state transition of
        Right spec ->
          as
            { state = nextCEMState spec
            , involvedActors =
                involvedActors as
                  <> Set.fromList (getAllSpecSigners spec)
            , finished = nextCEMState spec == Nothing
            }
        Left _ -> error "Unreachable"
      where
        nextCEMState spec = case outStates spec of
          [] -> Nothing
          [state'] -> Just state'
          _ ->
            error
              "This StateModel instance support only with single-output scripts"
        outStates spec = mapMaybe decodeOutState $ constraints spec
        decodeOutState c = case rest (txFanCFilter c) of
          UnsafeBySameCEM stateBS ->
            fromBuiltinData @(State script) stateBS
          _ -> Nothing
  nextState _ _ _ = error "Unreachable"

instance (CEMScriptArbitrary script) => Show (Action (ScriptState script) a) where
  show (ScriptTransition t) = "ScriptTransition " <> show t
  show (SetupActors {}) = "SetupActors"
  show (SetupCEMParams {}) = "SetupCEMParams"

deriving stock instance
  (CEMScriptArbitrary script) => Eq (Action (ScriptState script) a)

instance (CEMScriptArbitrary script) => DynLogicModel (ScriptState script)

-- RunModel

type instance Realized (ClbT m) () = ()

findSkForPKH :: [SigningKey PaymentKey] -> PubKeyHash -> SigningKey PaymentKey
findSkForPKH sks phk = head $ filter (\x -> signingKeyToPKH x == phk) sks

class (CEMScriptArbitrary script) => CEMScriptRunModel script where
  performHook ::
    (MonadIO m, MonadSubmitTx m) =>
    ScriptState script ->
    Action (ScriptState script) () ->
    m ()

instance
  ( Realized m () ~ ()
  , MonadIO m
  , MonadSubmitTx m
  , CEMScriptArbitrary script
  , CEMScriptRunModel script
  ) =>
  RunModel (ScriptState script) m
  where
  perform modelState action _lookup = do
    case (modelState, action) of
      (Void, SetupActors {}) -> do
        _ <- performHook modelState action
        return $ Right ()
      (ActorsKnown {}, SetupCEMParams {}) -> do
        _ <- performHook modelState action
        return $ Right ()
      (ScriptState {dappParams, state}, ScriptTransition transition) -> do
        _ <- performHook modelState action
        case transitionSpec (params dappParams) state transition of
          Right spec -> do
            r <-
              resolveTxAndSubmit $
                MkTxSpec
                  { actions =
                      [ MkSomeCEMAction $ MkCEMAction (cemParams dappParams) transition
                      ]
                  , specSigner = findSkForPKH (actors dappParams) $ signerPKH spec
                  }
            return $ bimap show (const ()) r
          Left err -> return $ Left $ show err
        where
          signerPKH spec = case getAllSpecSigners spec of
            [singleSigner] -> singleSigner
            _ -> error "Transition should have exactly one signer"
      (_, _) -> error "Unreachable"

  monitoring (stateFrom, stateTo) _ _ _ prop = do
    tabStateFrom $ labelIfFinished prop
    where
      isFinished (ScriptState {finished}) = finished
      isFinished _ = False
      labelIfFinished prop' =
        if isFinished stateTo
          then
            tabulate
              "Actors involved (in finished tests)"
              [show $ length $ involvedActors stateTo]
              $ label "Reached final state" prop'
          else prop'
      tabStateFrom prop' =
        case stateFrom of
          ScriptState {state} ->
            tabulate "States (from)" [show $ getSpine state] prop'
          _ -> prop'

  monitoringFailure state _ _ err prop =
    counterexample
      ( "In model state "
          <> ppShow state
          <> "\nGot error from emulator: "
          <> err
      )
      prop

runActionsInClb ::
  forall state.
  (StateModel (ScriptState state), RunModel (ScriptState state) (ClbT IO)) =>
  Value ->
  Actions (ScriptState state) ->
  Property
runActionsInClb genesisValue actions =
  monadic (ioProperty . execOnIsolatedClb genesisValue) $
    void $
      runActions @(ScriptState state) @(ClbT IO) actions

-- Orphans

instance HasVariables (SigningKey PaymentKey) where
  getAllVariables _ = Set.empty
