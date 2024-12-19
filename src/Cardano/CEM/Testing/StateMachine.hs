{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fewer imports" #-}

{- | Model-based testing based on `quickcheck-dynamic`.
Main purpose of this kind of testing is to ensure that
OnChain code works the same way OffChain code does.
Additinally custom user invariants for OffChain code
can be tested using 'CEMScriptRunModel' type class.
-}
module Cardano.CEM.Testing.StateMachine (
  -- * Model
  ScriptState (..),
  ScriptStateParams (..),
  TestConfig (..),
  Action (..),
  CEMScriptArbitrary (..),

  -- * SUT Implementation utils
  CEMScriptRunModel (..),
  runActionsInClb,
  findSkForPKH,
) where

import Cardano.Api (PaymentKey, SigningKey, TxId, Value)
import Cardano.CEM.DSL (
  CEMScript,
  CEMScriptTypes (Params, State, Transition),
  SameScriptArg (MkSameScriptArg),
  TxConstraint (Noop, Utxo),
  Utxo (SameScript),
  UtxoKind (Out),
  getMainSigner,
  getMbMainSigner,
 )
import Cardano.CEM.Monads (
  BlockchainMonadEvent (..),
  CEMAction (..),
  MonadBlockchainParams (..),
  MonadSubmitTx (..),
  ResolvedTx (..),
  SomeCEMAction (..),
  TxSpec (..),
 )
import Cardano.CEM.Monads.CLB (ClbRunner, execOnIsolatedClb)
import Cardano.CEM.OffChain (
  TxResolutionError (CEMScriptTxInResolutionError, UnhandledSubmittingError),
  compileActionConstraints,
  construct,
  process,
 )
import Cardano.CEM.OnChain (CEMScriptCompiled)
import Cardano.Extras (signingKeyToPKH)
import Clb (ClbT)
import Control.Monad (void)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans (MonadIO (..), MonadTrans (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Data (Typeable)
import Data.Either.Extra (mapLeft)
import Data.List (permutations)
import Data.Maybe (isJust, mapMaybe)
import Data.Set qualified as Set
import Data.Spine (HasSpine (..), deriveSpine)
import PlutusLedgerApi.V1 (PubKeyHash)
import Test.QuickCheck (Gen, Property, counterexample, ioProperty, label, tabulate)
import Test.QuickCheck.DynamicLogic (DynLogicModel)
import Test.QuickCheck.Gen qualified as Gen
import Test.QuickCheck.Monadic (monadic)
import Test.QuickCheck.StateModel (
  Actions,
  Any (..),
  Generic,
  HasVariables (..),
  LookUp,
  Realized,
  RunModel (..),
  StateModel (..),
  runActions,
 )
import Test.QuickCheck.StateModel.Variables (Var, VarContext)
import Text.Show.Pretty (ppShow)
import Prelude

-- -----------------------------------------------------------------------------
-- Mutations
-- -----------------------------------------------------------------------------

-- We use mutations to verify that on-chain and off-chain implementations
-- work the same way:
--  1. The order of constrainsts doesn't matter
--  2. All non-noop constraints are important - if we remove them both impls stop working.

data TxMutation
  = RemoveConstraint {num :: Int}
  | ShuffleConstraints {shift :: Int}
  deriving stock (Eq, Show)

deriveSpine ''TxMutation

isNegativeMutation :: Maybe TxMutation -> [TxConstraint True script] -> Bool
isNegativeMutation Nothing _ = False
isNegativeMutation m@(Just (RemoveConstraint {})) cs =
  case applyMutation m cs of
    (_, Just Noop) -> False
    _ -> True
isNegativeMutation (Just (ShuffleConstraints {})) _ = False

applyMutation ::
  Maybe TxMutation ->
  [TxConstraint True script] ->
  ([TxConstraint True script], Maybe (TxConstraint True script))
applyMutation Nothing cs = (cs, Nothing)
-- \| Removes num+1 element from the list of constraints
applyMutation (Just (RemoveConstraint num)) cs =
  (take num cs ++ tail (drop num cs), Just (cs !! num))
applyMutation (Just (ShuffleConstraints shift)) cs =
  (permute shift cs, Nothing)

permute :: Int -> [a] -> [a]
permute ind as =
  perms !! (ind `mod` n)
  where
    perms = permutations as
    n = length perms

-- -----------------------------------------------------------------------------
-- The model
-- -----------------------------------------------------------------------------

-- | Model: the ideal CEM script state.
data ScriptState script
  = Void
  | ConfigSet TestConfig
  | ScriptState
      { dappParams :: ScriptStateParams script
      , state :: Maybe (State script)
      , involvedActors :: Set.Set PubKeyHash
      , finished :: Bool
      }
  deriving stock (Generic)

deriving stock instance (CEMScript a) => Eq (ScriptState a)
deriving stock instance (CEMScript a) => Show (ScriptState a)

data ScriptStateParams script = MkScriptStateParams
  { config :: TestConfig
  , params :: Params script
  }
  deriving stock (Generic)

deriving stock instance (CEMScript a) => Eq (ScriptStateParams a)
deriving stock instance (CEMScript a) => Show (ScriptStateParams a)

data TestConfig = MkTestConfig
  { actors :: [SigningKey PaymentKey]
  , doMutationTesting :: Bool
  }
  deriving stock (Generic, Eq, Show)

-- We don't use symbolic variables so far.
instance HasVariables (ScriptState a) where
  getAllVariables _ = Set.empty

instance {-# OVERLAPS #-} HasVariables (Action (ScriptState script) a) where
  getAllVariables _ = Set.empty

-- -----------------------------------------------------------------------------
-- CEMScriptArbitrary & StateModel instance
-- -----------------------------------------------------------------------------

-- | Arbitrary for a CEM Script (compiled).
class (CEMScriptCompiled script) => CEMScriptArbitrary script where
  arbitraryParams ::
    [SigningKey PaymentKey] -> Gen (Params script)
  arbitraryTransition ::
    ScriptStateParams script -> Maybe (State script) -> Gen (Transition script)

{- | StateModel, which is QD model is basically our off-chain logic.
It delivers checks to `compileActionConstraints` from "Cardano.CEM.Offchain"
module. So `model === offchain`.
-}
instance (CEMScriptArbitrary script) => StateModel (ScriptState script) where
  data Action (ScriptState script) output where
    SetupConfig :: TestConfig -> Action (ScriptState script) ()
    SetupParams :: Params script -> Action (ScriptState script) ()
    ScriptTransition ::
      Transition script ->
      Maybe TxMutation ->
      Action (ScriptState script) ()
    deriving stock (Typeable)

  type Error (ScriptState script) = String

  initialState = Void

  actionName SetupConfig {} = "SetupConfig"
  actionName SetupParams {} = "SetupParams"
  actionName (ScriptTransition transition _) = head . words . show $ transition

  arbitraryAction ::
    (CEMScriptArbitrary script) =>
    VarContext ->
    ScriptState script ->
    Gen (Any (Action (ScriptState script)))
  arbitraryAction _varCtx modelState = case modelState of
    -- SetupConfig action should be always called manually
    Void {} -> Gen.oneof []
    ConfigSet config -> Some . SetupParams <$> arbitraryParams (actors config)
    ScriptState
      { dappParams =
        dappParams@MkScriptStateParams
          { config = MkTestConfig {doMutationTesting}
          }
      , state
      } ->
        do
          transition <- arbitraryTransition dappParams state
          Some <$> (ScriptTransition transition <$> genMutation transition)
        where
          genMutation :: Transition script -> Gen (Maybe TxMutation)
          genMutation transition =
            if doMutationTesting
              then mutate
              else return Nothing
            where
              mutate =
                let cemAction = MkCEMAction (params dappParams) transition
                 in case compileActionConstraints state cemAction of
                      Right cs ->
                        Gen.oneof
                          [ return Nothing
                          , Just . RemoveConstraint
                              <$> Gen.chooseInt (0, length cs - 1)
                          , Just
                              <$> ( ShuffleConstraints
                                      <$> Gen.chooseInt (1, length cs)
                                  )
                          ]
                      Left _ -> return Nothing

  precondition ::
    (CEMScriptArbitrary script) =>
    ScriptState script ->
    Action (ScriptState script) a ->
    Bool
  precondition Void (SetupConfig {}) = True
  precondition (ConfigSet {}) (SetupParams {}) = True
  precondition
    (ScriptState {dappParams, state, finished})
    (ScriptTransition transition mutation) =
      let
        cemAction = MkCEMAction (params dappParams) transition
        compiled = compileActionConstraints state cemAction
       in
        case compiled of
          Right cs -> not finished && not (isNegativeMutation mutation cs)
          Left _ -> False
  -- Unreachable
  precondition _ _ = False

  nextState ::
    (CEMScriptArbitrary script, Typeable a) =>
    ScriptState script ->
    Action (ScriptState script) a ->
    Var a ->
    ScriptState script
  nextState Void (SetupConfig config) _var = ConfigSet config
  nextState (ConfigSet config) (SetupParams params) _var =
    ScriptState
      { dappParams = MkScriptStateParams {config, params}
      , state = Nothing
      , involvedActors = Set.empty
      , finished = False
      }
  nextState
    as@ScriptState {dappParams, state}
    (ScriptTransition transition _mutation)
    _var =
      let
        cemAction = MkCEMAction (params dappParams) transition
        cs = case compileActionConstraints state cemAction of
          Right x -> x
          Left _ -> error "Unreachable: by preconditions"
       in
        as
          { state = nextCEMState cs
          , involvedActors =
              involvedActors as
                <> Set.fromList [getMainSigner cs]
          , finished = nextCEMState cs == Nothing
          }
      where
        nextCEMState cs = case mapMaybe f cs of
          [x] -> Just x
          [] -> Nothing
          _ ->
            error
              "Scripts with >1 SameScript outputs are not supported in CEM testing framework"
        f (Utxo Out (SameScript (MkSameScriptArg outState)) _) = Just outState
        f _ = Nothing
  nextState _ _ _ = error "Unreachable"

  -- Precondition for filtering an Action that can meaningfully run
  -- but is supposed to fail.
  -- An action will run as a _negative_ action if the 'precondition' fails and
  -- 'validFailingAction' succeeds.
  -- A negative action should have _no effect_ on the model state.
  -- This may not be desirable in all situations - in which case
  -- one can override this semantics for book-keeping in 'failureNextState'.
  validFailingAction ::
    (CEMScriptArbitrary script) =>
    ScriptState script ->
    Action (ScriptState script) a ->
    Bool
  validFailingAction
    (ScriptState {dappParams, finished, state})
    (ScriptTransition transition mutation) =
      let cemAction = MkCEMAction (params dappParams) transition
          cs' = compileActionConstraints state cemAction
       in case cs' of
            Left _ -> True
            Right cs -> isNegativeMutation mutation cs && isJust state && not finished
  validFailingAction _ _ = False

instance (CEMScriptArbitrary script) => Show (Action (ScriptState script) a) where
  show (ScriptTransition t m) = "ScriptTransition " <> show t <> " mutated as " <> show m
  show (SetupConfig {}) = "SetupConfig"
  show (SetupParams {}) = "SetupParams"

deriving stock instance
  (CEMScriptArbitrary script) => Eq (Action (ScriptState script) a)

instance (CEMScriptArbitrary script) => DynLogicModel (ScriptState script)

-- -----------------------------------------------------------------------------
-- RunModel
-- -----------------------------------------------------------------------------

type instance Realized (ClbT m) () = ()

findSkForPKH :: [SigningKey PaymentKey] -> PubKeyHash -> SigningKey PaymentKey
findSkForPKH sks phk = head $ filter (\x -> signingKeyToPKH x == phk) sks

class (CEMScriptArbitrary script) => CEMScriptRunModel script where
  performHook ::
    (MonadIO m, MonadSubmitTx m) =>
    ScriptState script ->
    Action (ScriptState script) () ->
    m ()

{- | The SUT implementation is CLB-backed blockchain emulator. Here we execute
both offchain part and the onchain part also. That way we can assume that
`implementation === onchain`.
-}
instance
  ( Realized m () ~ ()
  , MonadIO m
  , MonadSubmitTx m
  , CEMScriptArbitrary script
  , CEMScriptRunModel script
  ) =>
  RunModel (ScriptState script) m
  where
  perform ::
    ( Realized m () ~ ()
    , MonadIO m
    , MonadSubmitTx m
    , CEMScriptArbitrary script
    , CEMScriptRunModel script
    , Typeable a
    ) =>
    ScriptState script ->
    Action (ScriptState script) a ->
    LookUp m ->
    m (Either (Error (ScriptState script)) (Realized m a))
  perform modelState action _lookup = do
    case (modelState, action) of
      (Void, SetupConfig {}) -> do
        _ <- performHook modelState action
        return $ Right ()
      (ConfigSet {}, SetupParams {}) -> do
        _ <- performHook modelState action
        return $ Right ()
      ( ScriptState {dappParams, state}
        , ScriptTransition transition mutation
        ) -> do
          _ <- performHook modelState action
          bimap show (const ()) <$> mutateResolveAndSubmit
          where
            mutateResolveAndSubmit :: m (Either TxResolutionError TxId)
            mutateResolveAndSubmit = runExceptT $ do
              let cemAction = MkCEMAction (params dappParams) transition
              cs' <- ExceptT $ return $ compileActionConstraints state cemAction
              let
                (cs, _) = applyMutation mutation cs'
                mbSignerPKH = getMbMainSigner cs
              -- \| FIXME: can we delegate handling Nothing case to process/construct?
              specSigner <- case mbSignerPKH of
                Nothing -> ExceptT $ pure $ Left CEMScriptTxInResolutionError -- FIXME:
                Just signerPKH -> pure $ findSkForPKH (actors $ config dappParams) signerPKH
              resolutions <- mapM (process cemAction) cs
              let resolvedTx = (construct resolutions) {signer = specSigner}
              result <-
                first UnhandledSubmittingError
                  <$> lift (submitResolvedTx resolvedTx)
              let spec = MkTxSpec [MkSomeCEMAction cemAction] specSigner
              lift $
                logEvent $
                  SubmittedTxSpec spec (mapLeft (const ()) result)
              ExceptT $ return result
      (_, _) -> error "Unreachable branch of `perform`"

  monitoring ::
    ( Realized m () ~ ()
    , MonadIO m
    , MonadSubmitTx m
    , CEMScriptArbitrary script
    , CEMScriptRunModel script
    ) =>
    (ScriptState script, ScriptState script) ->
    Action (ScriptState script) a ->
    LookUp m ->
    Either (Error (ScriptState script)) (Realized m a) ->
    Property ->
    Property
  monitoring (stateFrom, stateTo) action _ _ prop = do
    tabMutations $ tabStateFrom $ labelIfFinished prop
    where
      isFinished (ScriptState {finished}) = finished
      isFinished _ = False
      labelIfFinished :: Property -> Property
      labelIfFinished prop' =
        if isFinished stateTo
          then
            tabulate
              "Actors involved (in finished tests)"
              [show $ length $ involvedActors stateTo]
              $ label "Reached final state" prop'
          else prop'
      tabStateFrom :: Property -> Property
      tabStateFrom prop' =
        case stateFrom of
          ScriptState {state} ->
            tabulate "States (from)" [show $ getSpine state] prop'
          _ -> prop'
      tabMutations :: Property -> Property
      tabMutations prop' =
        case (stateFrom, action) of
          (ScriptState {dappParams}, ScriptTransition _ mutation)
            | doMutationTesting $ config dappParams ->
                tabulate "Mutations" [show $ getSpine mutation] prop'
          _ -> prop'

  monitoringFailure state action _ err prop =
    counterexample
      ( "Failed action is:\n"
          <> ppShow action
          <> "In model state:\n"
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
      runActions @(ScriptState state) @ClbRunner actions

-- Orphans

instance HasVariables (SigningKey PaymentKey) where
  getAllVariables _ = Set.empty
