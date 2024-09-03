{-# OPTIONS_GHC -Wno-orphans #-}

{- | Generic utils for using `quickcheck-dynamic`
FIXME: refactor and add documentation
-}
module Cardano.CEM.Testing.StateMachine where

import Prelude

import Control.Monad (void)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans (MonadIO (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Data (Typeable)
import Data.List (permutations)
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
import Cardano.CEM.Monads (CEMAction (..), MonadSubmitTx (..), ResolvedTx (..), SomeCEMAction (..), TxSpec (..))
import Cardano.CEM.Monads.CLB (ClbRunner, execOnIsolatedClb)
import Cardano.CEM.OffChain
import Cardano.CEM.OnChain (CEMScriptCompiled)
import Cardano.Extras (signingKeyToPKH)
import Data.Spine (HasSpine (..), deriveSpine)

-- FIXME: add more mutations and documentation
data TxMutation = RemoveTxFan TxFanKind | ShuffleTxFan TxFanKind Int
  deriving stock (Eq, Show)

deriveSpine ''TxMutation

isNegativeMutation :: Maybe TxMutation -> Bool
isNegativeMutation Nothing = False
isNegativeMutation (Just (RemoveTxFan _)) = True
isNegativeMutation (Just (ShuffleTxFan {})) = False

permute :: Int -> [a] -> [a]
permute num arr =
  pms !! (num `mod` length pms)
  where
    pms = permutations arr

applyMutation :: Maybe TxMutation -> ResolvedTx -> ResolvedTx
applyMutation Nothing tx = tx
applyMutation (Just (RemoveTxFan In)) tx = tx {txIns = tail $ txIns tx}
applyMutation (Just (RemoveTxFan Out)) tx = tx {txOuts = tail $ txOuts tx}
applyMutation (Just (RemoveTxFan InRef)) tx =
  tx {txInsReference = tail $ txInsReference tx}
applyMutation (Just (ShuffleTxFan In num)) tx =
  tx {txIns = permute num $ txIns tx}
applyMutation (Just (ShuffleTxFan Out num)) tx =
  tx {txOuts = permute num $ txOuts tx}
applyMutation (Just (ShuffleTxFan InRef num)) tx =
  tx {txInsReference = permute num $ txInsReference tx}

data TestConfig = MkTestConfig
  { actors :: [SigningKey PaymentKey]
  , doMutationTesting :: Bool
  }
  deriving stock (Generic, Eq, Show)

data ScriptStateParams a = MkScriptStateParams
  { config :: TestConfig
  , cemParams :: CEMParams a
  }
  deriving stock (Generic)

params :: ScriptStateParams script -> Params script
params = scriptParams . cemParams

deriving stock instance (CEMScript a) => Eq (ScriptStateParams a)
deriving stock instance (CEMScript a) => Show (ScriptStateParams a)

data ScriptState a
  = Void
  | ConfigSet TestConfig
  | ScriptState
      { dappParams :: ScriptStateParams a
      , state :: Maybe (State a)
      , involvedActors :: Set.Set PubKeyHash
      , finished :: Bool
      }
  deriving stock (Generic)

deriving stock instance (CEMScript a) => Eq (ScriptState a)
deriving stock instance (CEMScript a) => Show (ScriptState a)

instance HasVariables (ScriptState a) where
  getAllVariables _ = Set.empty

instance {-# OVERLAPS #-} HasVariables (Action (ScriptState script) a) where
  getAllVariables _ = Set.empty

class
  (CEMScriptCompiled script) =>
  CEMScriptArbitrary script
  where
  arbitraryCEMParams :: [SigningKey PaymentKey] -> Gen (CEMParams script)
  arbitraryTransition ::
    ScriptStateParams script -> Maybe (State script) -> Gen (Transition script)

instance (CEMScriptArbitrary script) => StateModel (ScriptState script) where
  data Action (ScriptState script) output where
    SetupConfig :: TestConfig -> Action (ScriptState script) ()
    SetupCEMParams :: CEMParams script -> Action (ScriptState script) ()
    ScriptTransition ::
      Transition script ->
      Maybe TxMutation ->
      Action (ScriptState script) ()
    deriving stock (Typeable)

  type Error (ScriptState script) = String

  initialState = Void

  actionName (ScriptTransition transition _) = head . words . show $ transition
  actionName SetupConfig {} = "SetupConfig"
  actionName SetupCEMParams {} = "SetupCEMParams"

  arbitraryAction _varCtx modelState = case modelState of
    -- SetupConfig action should be called manually
    Void {} -> Gen.oneof []
    ConfigSet config ->
      Some . SetupCEMParams <$> arbitraryCEMParams (actors config)
    ScriptState {dappParams, state} ->
      do
        transition <- arbitraryTransition dappParams state
        Some <$> (ScriptTransition transition <$> genMutation transition)
      where
        genTxKind = Gen.elements [In, Out]
        genMutation transition =
          if not $ doMutationTesting $ config dappParams
            then return Nothing
            else case transitionSpec @script (params dappParams) state transition of
              Left _ -> return Nothing
              Right _spec ->
                Gen.oneof
                  [ return Nothing
                  , Just . RemoveTxFan <$> genTxKind
                  , Just
                      <$> ( ShuffleTxFan
                              <$> genTxKind
                              <*> Gen.chooseInt (1, 10)
                          )
                  ]

  precondition Void (SetupConfig {}) = True
  precondition (ConfigSet {}) (SetupCEMParams {}) = True
  precondition
    (ScriptState {dappParams, state, finished})
    (ScriptTransition transition mutation) =
      case transitionSpec @script (params dappParams) state transition of
        Right _ ->
          not finished && not (isNegativeMutation mutation)
        Left _ -> False
  -- Unreachable
  precondition _ _ = False

  -- XXX: Check on ScriptState and it fields is required for shrinking
  -- FIXME: docs on QD generation hacks
  validFailingAction (ScriptState {finished, state}) (ScriptTransition _ mutation) =
    isNegativeMutation mutation && state /= Nothing && not finished
  validFailingAction _ _ = False

  nextState Void (SetupConfig config) _var = ConfigSet config
  nextState (ConfigSet config) (SetupCEMParams cemParams) _var =
    ScriptState
      { dappParams = MkScriptStateParams {config, cemParams}
      , state = Nothing
      , involvedActors = Set.empty
      , finished = False
      }
  nextState
    as@ScriptState {dappParams, state}
    (ScriptTransition transition _mutation)
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
        decodeOutState c = case rest (txFansCFilter c) of
          UnsafeBySameCEM stateBS ->
            fromBuiltinData @(State script) (unAsData stateBS)
          _ -> Nothing
  nextState _ _ _ = error "Unreachable"

instance (CEMScriptArbitrary script) => Show (Action (ScriptState script) a) where
  show (ScriptTransition t m) = "ScriptTransition " <> show t <> " mutated as " <> show m
  show (SetupConfig {}) = "SetupConfig"
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
      (Void, SetupConfig {}) -> do
        _ <- performHook modelState action
        return $ Right ()
      (ConfigSet {}, SetupCEMParams {}) -> do
        _ <- performHook modelState action
        return $ Right ()
      ( ScriptState {dappParams, state}
        , ScriptTransition transition mutation
        ) -> do
          _ <- performHook modelState action
          case transitionSpec (params dappParams) state transition of
            Right spec -> do
              r <- runExceptT $ do
                resolved <-
                  ExceptT $
                    first show
                      <$> ( resolveTx $
                              MkTxSpec
                                { actions =
                                    [ MkSomeCEMAction $ MkCEMAction (cemParams dappParams) transition
                                    ]
                                , specSigner =
                                    findSkForPKH (actors $ config dappParams) $ signerPKH spec
                                }
                          )
                ExceptT $
                  first show
                    <$> submitResolvedTx (applyMutation mutation resolved)
              return $ second (const ()) r
            Left err -> return $ Left $ show err
          where
            signerPKH spec = case getAllSpecSigners spec of
              [singleSigner] -> singleSigner
              _ -> error "Transition should have exactly one signer"
      (_, _) -> error $ "Unreachable"

  monitoring (stateFrom, stateTo) action _ _ prop = do
    tabMutations $ tabStateFrom $ labelIfFinished prop
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
      tabMutations prop' =
        case (stateFrom, action) of
          (ScriptState {dappParams}, ScriptTransition _ mutation)
            | doMutationTesting $ config dappParams ->
                tabulate "Mutations" [show $ getSpine mutation] prop'
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
  (StateModel (ScriptState state), RunModel (ScriptState state) ClbRunner) =>
  Value ->
  Actions (ScriptState state) ->
  Property
runActionsInClb genesisValue actions =
  monadic (ioProperty . execOnIsolatedClb genesisValue) $
    void $
      runActions @(ScriptState state) @(ClbRunner) actions

-- Orphans

instance HasVariables (SigningKey PaymentKey) where
  getAllVariables _ = Set.empty
