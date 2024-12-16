{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Indexer events, i.e. indexer outputs.
module Cardano.CEM.Indexing.Event (
  IndexerEvent (..),
  extractEvent,
) where

import Cardano.Api qualified as C
import Cardano.Api.ScriptData qualified as C
import Cardano.Api.SerialiseRaw qualified as SerialiseRaw
import Cardano.CEM (CEMScript, CEMScriptDatum, State, Transition, allTransitions)
import Cardano.CEM.Address qualified as Address
import Cardano.CEM.Indexing.Tx
import Cardano.CEM.OnChain (CEMScriptCompiled)
import Cardano.Ledger.BaseTypes qualified as Ledger
import Control.Lens (view, (^.))
import Data.ByteString.Base16 qualified as B16
import Data.Data (Proxy (Proxy))
import Data.Either.Extra (eitherToMaybe)
import Data.Function ((&))
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Spine (Spine, getSpine)
import Data.Text.Encoding (encodeUtf8)
import Data.Tuple (swap)
import PlutusLedgerApi.V1 (FromData)
import PlutusLedgerApi.V1 qualified
import Prelude

-- ---

{- | Indexer events.
  We extract events from transactions, where we can encounter three situations:

  (1) For the very first transition there is only target datum and no redeemer.
  In that case we can only restore the name of the transition,
  i.e. 'Spine Transition'

  (2) For intermidiate transitions we have both datums that identify them and
  additionally redeemer, that contains the whole transition. In that case
  we can restore the whole transition.

  (3) For the final transition the situation is like (2) except the target
  datum is missing, which doesn't matter.

  TODO: How we can improve this in the future:
  * API is probably bad, as we always have some transition like Init state -
  which you can decode, as you have State. If one changes data
  `CEMAction script = MkCEMAction (Params script) (Transition script)` to
  `... = Init (Params script) (State script)
       | Transition (Params script) (Transition script)`
  one could reuse this datatype in all situations.
-}
data IndexerEvent script
  = Initial (Spine (Transition script))
  | -- | TODO: Migrate from (Spine (Transition script)) to (Transition script)
    -- once we have this done: https://github.com/utxorpc/spec/issues/132
    Following (Spine (Transition script)) -- (Transition script)

deriving stock instance
  (Show (Spine (Transition script))) =>
  (Show (IndexerEvent script))
deriving stock instance
  (Eq (Spine (Transition script))) =>
  (Eq (IndexerEvent script))

{- | The core function, that extracts an 'Event' out of a Oura transaction.
It might be a pure function, IO here was used mostly to simplify debugging
during its development.
-}
extractEvent ::
  forall script.
  ( CEMScript script
  , CEMScriptCompiled script
  ) =>
  Ledger.Network ->
  Tx ->
  IO (Maybe (IndexerEvent script))
extractEvent network tx = do
  -- Script payment credential based predicate
  let ~(Right scriptAddr) = Address.cemScriptAddress network (Proxy @script)
  let cPred = hasAddr scriptAddr

  -- Source state
  let mOwnInput :: Maybe TxInput = find (cPred . view as_output) (tx ^. inputs)
  let mSourceState :: Maybe (State script) = (extractState . view as_output) =<< mOwnInput
  let mSourceSpine :: Maybe (Spine (State script)) = getSpine <$> mSourceState

  -- Target state
  let mOwnOutput :: Maybe TxOutput = find cPred $ tx ^. outputs
  let mTargetState :: Maybe (State script) = extractState =<< mOwnOutput
  let mTargetSpine :: Maybe (Spine (State script)) = getSpine <$> mTargetState

  -- Look up the transition
  let transitions = swap <$> Map.toList allTransitions
  let transSpine = lookup (mSourceSpine, mTargetSpine) transitions

  -- Return
  case mOwnInput of
    Nothing -> pure $ Initial <$> transSpine
    Just _ownInput -> do
      -- TODO: fix once Oura has rawCbor for redeemer
      -- rdm <- ownInput ^. redeemer
      -- pure $ Following $ undefined (rdm ^. redeemerPayload)
      pure $ Following <$> transSpine

extractState ::
  forall script.
  (FromData (CEMScriptDatum script)) =>
  TxOutput ->
  Maybe (State script)
extractState MkTxOutput {_datum = mDtm} =
  case mDtm of
    Nothing -> Nothing
    Just dtm -> do
      let MkDatum _ _ cbor = dtm
      let datumAsData :: PlutusLedgerApi.V1.Data =
            cbor
              & C.toPlutusData
                . C.getScriptData
                . fromJust
                . eitherToMaybe
                . C.deserialiseFromCBOR C.AsHashableScriptData
                . B16.decodeBase16Lenient -- use base64
                . encodeUtf8
      let ~(Just (_, state)) = PlutusLedgerApi.V1.fromData @(CEMScriptDatum script) datumAsData
      pure state

hasAddr :: C.Address C.ShelleyAddr -> TxOutput -> Bool
hasAddr addr' output =
  let addr = output ^. address
   in fromOuraAddress addr == addr'

fromOuraAddress :: Address -> C.Address C.ShelleyAddr
fromOuraAddress (MkAddressAsBase64 addr) =
  addr
    & fromJust
      . eitherToMaybe
      . SerialiseRaw.deserialiseFromRawBytes (C.AsAddress C.AsShelleyAddr)
      . B16.decodeBase16Lenient -- use base64
      . encodeUtf8
