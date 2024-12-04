{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module OuraFilters.Mock where

import Cardano.Api qualified as C

-- import Cardano.Api.Address qualified as C
import Cardano.Api (TxBody, TxIn, UTxO)
import Cardano.Api.Address qualified as C (Address (..))
import Cardano.Api.ScriptData qualified as C
import Cardano.Api.SerialiseRaw qualified as SerialiseRaw
import Cardano.CEM (CEMScript, CEMScriptDatum, State, Transition, transitionStage)
import Cardano.CEM.Address qualified as Address
import Cardano.CEM.Monads (ResolvedTx (..))
import Cardano.CEM.OnChain (CEMScriptCompiled, CEMScriptIsData)
import Cardano.Extras (Era, TxInWitness)
import Cardano.Ledger.BaseTypes qualified as Ledger
import Control.Lens (preview, view, (^.))
import Control.Lens.TH (makeLenses, makeLensesFor)
import Control.Monad ((<=<))
import Control.Monad.Extra (join)
import Data.Aeson (KeyValue ((.=)))
import Data.Aeson qualified as Aeson
import Data.Base16.Types qualified as Base16
import Data.Base16.Types qualified as Base16.Types
import Data.Base64.Types qualified as Base64
import Data.Base64.Types qualified as Base64.Types
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Base64.URL qualified as B64
import Data.ByteString.Lazy qualified as LBS
import Data.Data (Proxy (Proxy))
import Data.Either (fromRight)
import Data.Either.Extra (eitherToMaybe)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Data.Spine (Spine, getSpine)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Tuple (swap)
import Data.Vector qualified as Vec
import Debug.Trace (trace, traceShowId)
import GHC.Generics (Generic (Rep))
import GHC.Stack.Types (HasCallStack)
import PlutusLedgerApi.V1 (Credential, FromData, ToData)
import PlutusLedgerApi.V1 qualified
import Safe qualified
import System.Process.Internals (ProcRetHandles (hStdOutput))
import Test.QuickCheck (Result (output))
import Utils (digits)
import Prelude

newtype WithoutUnderscore a = MkWithoutUnderscore a
  deriving newtype (Generic)

withoutLeadingUnderscore :: Aeson.Options
withoutLeadingUnderscore =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = \case
        '_' : fieldName -> fieldName
        fieldName -> fieldName
    }
instance
  ( Generic a
  , Aeson.GToJSON' Aeson.Value Aeson.Zero (GHC.Generics.Rep a)
  ) =>
  Aeson.ToJSON (WithoutUnderscore a)
  where
  toJSON = Aeson.genericToJSON withoutLeadingUnderscore

instance (Generic a, Aeson.GFromJSON Aeson.Zero (Rep a)) => Aeson.FromJSON (WithoutUnderscore a) where
  parseJSON = Aeson.genericParseJSON withoutLeadingUnderscore

newtype Address = MkAddressAsBase64 {_addressL :: T.Text}
  deriving newtype (Show, Eq, Ord, Aeson.ToJSON, Aeson.FromJSON)
makeLenses ''Address

-- 32B long
newtype Hash32 = MkBlake2b255Hex {unHash32 :: T.Text}
  deriving newtype (Show, Eq, Ord)
  deriving newtype (Aeson.ToJSON)
  deriving newtype (Aeson.FromJSON)
makeLenses ''Hash32

-- 28B long
newtype Hash28 = MkBlake2b244Hex {unHash28 :: T.Text}
  deriving newtype (Show, Eq, Ord)
  deriving newtype (Aeson.ToJSON)
  deriving newtype (Aeson.FromJSON)
makeLenses ''Hash28

data Asset = MkAsset
  { _name :: T.Text
  , _output_coin :: Integer -- positive
  , _mint_coin :: Integer
  }
  deriving stock (Generic)
  deriving (Aeson.ToJSON) via (WithoutUnderscore Asset)
  deriving (Aeson.FromJSON) via (WithoutUnderscore Asset)
makeLenses ''Asset

data Multiasset = MkMultiasset
  { _policy_id :: Hash28
  , assets :: [Asset]
  }
  deriving stock (Generic)
  deriving (Aeson.ToJSON) via (WithoutUnderscore Multiasset)
  deriving (Aeson.FromJSON) via (WithoutUnderscore Multiasset)
makeLenses ''Multiasset
makeLensesFor
  [ ("assets", "multiassetAssets")
  -- , ("redeemer", "multiassetRedeemer")
  ]
  ''Multiasset

newtype PlutusData = MkPlutusData {_plutusData :: Aeson.Value}
  deriving newtype (Generic)
  deriving newtype (Aeson.FromJSON, Aeson.ToJSON)
makeLenses ''PlutusData

data Purpose
  = PURPOSE_UNSPECIFIED
  | PURPOSE_SPEND
  | PURPOSE_MINT
  | PURPOSE_CERT
  | PURPOSE_REWARD
  deriving stock (Show, Enum, Bounded)

instance Aeson.FromJSON Purpose where
  parseJSON =
    maybe (fail "There is no Purpose case with this Id") pure
      . Safe.toEnumMay
      <=< Aeson.parseJSON @Int

instance Aeson.ToJSON Purpose where
  toJSON = Aeson.toJSON @Int . fromEnum

data Datum = MkDatum
  { hash :: Hash32
  , _payload :: PlutusData
  , _original_cbor :: T.Text
  }
  deriving stock (Generic)
  deriving (Aeson.ToJSON) via (WithoutUnderscore Datum)
  deriving (Aeson.FromJSON) via (WithoutUnderscore Datum)
makeLenses ''Datum
makeLensesFor [("hash", "datumHash")] ''Datum

data Redeemer = MkRedeemer
  { _purpose :: Purpose
  , payload :: PlutusData
  }
  deriving stock (Generic)
  deriving (Aeson.ToJSON) via (WithoutUnderscore Redeemer)
  deriving (Aeson.FromJSON) via (WithoutUnderscore Redeemer)
makeLenses ''Redeemer
makeLensesFor [("payload", "redeemerPayload")] ''Redeemer

data TxOutput = MkTxOutput
  { _address :: Address
  , _coin :: Integer
  , _assets :: [Multiasset]
  , _datum :: Maybe Datum
  , _script :: Maybe Aeson.Value
  }
  deriving stock (Generic)
  deriving (Aeson.ToJSON) via (WithoutUnderscore TxOutput)
  deriving (Aeson.FromJSON) via (WithoutUnderscore TxOutput)
makeLenses ''TxOutput

data TxInput = MkTxInput
  { _tx_hash :: Hash32
  , _output_index :: Integer
  , _as_output :: TxOutput
  , _redeemer :: Maybe Redeemer
  }
  deriving stock (Generic)
  deriving (Aeson.ToJSON) via (WithoutUnderscore TxInput)
  deriving (Aeson.FromJSON) via (WithoutUnderscore TxInput)
makeLenses ''TxInput

data TxWitnesses = MkTxWitnesses
  { _vkeywitness :: [Aeson.Value]
  , script :: [Aeson.Value]
  , _plutus_datums :: [Aeson.Value]
  }
  deriving stock (Generic)
  deriving (Aeson.ToJSON) via (WithoutUnderscore TxWitnesses)
  deriving (Aeson.FromJSON) via (WithoutUnderscore TxWitnesses)

makeLenses ''TxWitnesses
makeLensesFor [("script", "txWitnessesScript")] ''Multiasset

data TxCollateral = MkTxCollateral
  { _collateral :: [Aeson.Value]
  , _collateral_return :: TxOutput
  , _total_collateral :: Integer
  }
  deriving stock (Generic)
  deriving (Aeson.ToJSON) via (WithoutUnderscore TxCollateral)
  deriving (Aeson.FromJSON) via (WithoutUnderscore TxCollateral)
makeLenses ''TxCollateral

data TxValidity = MkTxValidity
  { _start :: Integer
  , _ttl :: Integer
  }
  deriving stock (Generic)
  deriving (Aeson.ToJSON) via (WithoutUnderscore TxValidity)
  deriving (Aeson.FromJSON) via (WithoutUnderscore TxValidity)
makeLenses ''TxValidity

data TxAuxiliary = MkTxAuxiliary
  { _metadata :: [Aeson.Value]
  , _scripts :: [Aeson.Value]
  }
  deriving stock (Generic)
  deriving (Aeson.ToJSON) via (WithoutUnderscore TxAuxiliary)
  deriving (Aeson.FromJSON) via (WithoutUnderscore TxAuxiliary)
makeLenses ''TxAuxiliary

arbitraryTx :: Tx
arbitraryTx =
  MkTx
    { _inputs = []
    , _outputs = []
    , _certificates = []
    , _withdrawals = []
    , _mint = []
    , _reference_inputs = []
    , _witnesses =
        MkTxWitnesses
          { _vkeywitness = []
          , script = []
          , _plutus_datums = []
          }
    , collateral =
        MkTxCollateral
          { _collateral = []
          , _collateral_return =
              MkTxOutput
                { _address = MkAddressAsBase64 "cM+tGRS1mdGL/9FNK71pYBnCiZy91qAzJc32gLw="
                , _coin = 0
                , _assets = []
                , _datum = Nothing
                , _script = Nothing
                }
          , _total_collateral = 0
          }
    , _fee = 0
    , _validity =
        MkTxValidity
          { _start = 0
          , _ttl = 0
          }
    , _successful = True
    , _auxiliary =
        MkTxAuxiliary
          { _metadata = []
          , _scripts = []
          }
    , _hash = MkBlake2b255Hex "af6366838cfac9cc56856ffe1d595ad1dd32c9bafb1ca064a08b5c687293110f"
    }

-- Source: https://docs.rs/utxorpc-spec/latest/utxorpc_spec/utxorpc/v1alpha/cardano/struct.Tx.html
data Tx = MkTx
  { _inputs :: [TxInput]
  , _outputs :: [TxOutput]
  , _certificates :: [Aeson.Value]
  , _withdrawals :: [Aeson.Value]
  , _mint :: [Aeson.Value]
  , _reference_inputs :: [Aeson.Value]
  , _witnesses :: TxWitnesses
  , collateral :: TxCollateral
  , _fee :: Integer
  , _validity :: TxValidity
  , _successful :: Bool
  , _auxiliary :: TxAuxiliary
  , _hash :: Hash32
  }
  deriving stock (Generic)
  deriving (Aeson.ToJSON) via (WithoutUnderscore Tx)
  deriving (Aeson.FromJSON) via (WithoutUnderscore Tx)
makeLenses ''Tx
makeLensesFor [("collateral", "txCollateral")] ''Tx

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
  | -- | FIXME: Migrate from (Spine (Transition script)) to (Transition script)
    -- | FIXME: Open an issue in Oura's repository
    Following (Spine (Transition script)) -- (Transition script)

deriving stock instance (Show (Spine (Transition script))) => (Show (IndexerEvent script))

-- For testing: build a tx in the Oura format from a Cardano tx.
-- We populate only fields we use, use with cautious.
resolvedTxToOura :: C.TxBodyContent C.BuildTx Era -> UTxO Era -> Tx
resolvedTxToOura tbc utxo =
  arbitraryTx
    { _inputs = oInputs
    , _outputs = oOutputs
    }
  where
    oInputs = mapMaybe (toOuraInput utxo . fst) (C.txIns tbc)
    oOutputs = toOuraTxOutput <$> C.txOuts tbc

-- | This is a partial function, use with cautious
toOuraInput :: UTxO Era -> TxIn -> Maybe TxInput
toOuraInput (C.UTxO utxo) txIn =
  case Map.lookup txIn utxo of
    Nothing -> Nothing
    Just output ->
      pure $
        MkTxInput
          { _tx_hash = MkBlake2b255Hex ""
          , _output_index = 0
          , _as_output = toOuraTxOutput output
          , _redeemer = Nothing
          }

-- | This is a partial function, we use address and datum
toOuraTxOutput :: C.TxOut ctx Era -> TxOutput
toOuraTxOutput (C.TxOut addr _ dat _) =
  MkTxOutput
    { _address = toOuraAddrress addr
    , _coin = 0
    , _assets = []
    , _datum = toOuraDatum dat
    , _script = Nothing
    }

-- | This is a partial function, we use only original_cbor.
toOuraDatum :: C.TxOutDatum ctx Era -> Maybe Datum
toOuraDatum = \case
  (C.TxOutDatumInline _ hsd) ->
    let bs = traceShowId $ C.serialiseToCBOR hsd
     in Just $
          MkDatum
            { _payload = MkPlutusData Aeson.Null
            , hash = MkBlake2b255Hex ""
            , _original_cbor -- Base64.extractBase64 $ Base64.encodeBase64 $
              =
                Base16.extractBase16 $ Base16.encodeBase16 bs
            }
  _ -> Nothing

toOuraAddrress :: C.AddressInEra Era -> Address
toOuraAddrress (C.AddressInEra _ addr) =
  case addr of
    C.ByronAddress _ -> error "Encounter Byron address"
    C.ShelleyAddress {} ->
      addr
        & MkAddressAsBase64
          -- . Base64.extractBase64
          -- . Base64.encodeBase64
          . Base16.extractBase16
          . Base16.encodeBase16
          . SerialiseRaw.serialiseToRawBytes

-- The core function, that extracts an Event out of a Oura transaction.
extractEvent ::
  forall script.
  ( CEMScript script
  , CEMScriptIsData script
  , CEMScriptCompiled script
  ) =>
  Tx ->
  Ledger.Network ->
  IO (Maybe (IndexerEvent script))
extractEvent tx network = do
  -- Script payemnt credential based predicate
  let (Right scriptAddr) = Address.scriptCardanoAddress (Proxy @script) network
  let cPred = hasAddr scriptAddr
  print scriptAddr

  -- Source state
  let mOwnInput :: Maybe TxInput = find (cPred . view as_output) (tx ^. inputs)
  let mSourceState :: Maybe (State script) = (extractState . view as_output) =<< mOwnInput
  let mSourceSpine :: Maybe (Spine (State script)) = getSpine <$> mSourceState
  putStr "Source state: "
  print mSourceSpine

  -- Target state
  let mOwnOutput :: Maybe TxOutput = find cPred $ tx ^. outputs
  let mTargetState :: Maybe (State script) = extractState =<< mOwnOutput
  let mTargetSpine :: Maybe (Spine (State script)) = getSpine <$> mTargetState
  putStr "Target state: "
  print mTargetSpine

  -- Look up the transition
  let transitions =
        first
          (\(_, b, c) -> (b, c))
          . swap
          <$> Map.toList (transitionStage $ Proxy @script)
  let transSpine = lookup (mSourceSpine, mTargetSpine) transitions

  -- Return
  case mOwnInput of
    Nothing -> pure $ Initial <$> transSpine
    Just _ownInput -> do
      -- FIXME: fix once Oura has rawCbor for redeemer
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
            traceShowId cbor
              & C.toPlutusData
                . C.getScriptData
                . fromJust
                . eitherToMaybe
                . C.deserialiseFromCBOR C.AsHashableScriptData
                . Base16.decodeBase16Lenient
                . encodeUtf8
      let ~(Just (_, _, state)) = PlutusLedgerApi.V1.fromData @(CEMScriptDatum script) datumAsData
      pure state

hasAddr :: C.Address C.ShelleyAddr -> TxOutput -> Bool
hasAddr addr' output =
  let addr = output ^. address
   in traceShowId (fromOuraAddress addr) == addr'

fromOuraAddress :: Address -> C.Address C.ShelleyAddr
fromOuraAddress (MkAddressAsBase64 addr) =
  addr
    & fromJust
      . eitherToMaybe
      . SerialiseRaw.deserialiseFromRawBytes (C.AsAddress C.AsShelleyAddr)
      --  . fromJust
      --  . eitherToMaybe
      --  . B64.decodeBase64PaddedUntyped
      . Base16.decodeBase16Lenient
      . encodeUtf8

data TxEvent = MkTxEvent
  { _parsed_tx :: Tx
  , _point :: String -- "Origin"
  }
  deriving stock (Generic)
  deriving (Aeson.ToJSON) via (WithoutUnderscore TxEvent)
  deriving (Aeson.FromJSON) via (WithoutUnderscore TxEvent)
makeLenses ''TxEvent

mkTxEvent :: Tx -> TxEvent
mkTxEvent _parsed_tx =
  MkTxEvent
    { _parsed_tx
    , _point = "Origin"
    }

txToBS :: TxEvent -> BS.ByteString
txToBS = LBS.toStrict . Aeson.encode

encodePlutusData :: PlutusLedgerApi.V1.Data -> PlutusData
encodePlutusData = MkPlutusData . datumToJson

datumToJson :: PlutusLedgerApi.V1.Data -> Aeson.Value
{-# NOINLINE datumToJson #-}
datumToJson =
  \case
    PlutusLedgerApi.V1.Constr n fields ->
      Aeson.object
        [ "constr"
            .= Aeson.object
              [ "tag" .= Aeson.Number (fromInteger n)
              , "any_constructor" .= Aeson.Number 0
              , "fields"
                  .= Aeson.Array
                    (Vec.fromList $ datumToJson <$> fields)
              ]
        ]
    PlutusLedgerApi.V1.Map kvs ->
      Aeson.object
        [ "map"
            .= Aeson.object
              [ "pairs"
                  .= Aeson.Array
                    ( Vec.fromList $
                        kvs <&> \(k, v) ->
                          Aeson.object
                            [ "key" .= datumToJson k
                            , "value" .= datumToJson v
                            ]
                    )
              ]
        ]
    PlutusLedgerApi.V1.I n ->
      Aeson.object
        [ "big_int"
            .= Aeson.object
              [ "big_n_int"
                  .= Aeson.String
                    ( Base64.Types.extractBase64 $
                        Base64.encodeBase64 $
                          BS.pack $
                            fromInteger
                              <$> digits @Integer @Double 16 n
                    )
              ]
        ]
    PlutusLedgerApi.V1.B bs ->
      Aeson.object
        [ "bounded_bytes"
            .= Aeson.String
              ( Base64.Types.extractBase64 $
                  Base64.encodeBase64 bs
              )
        ]
    PlutusLedgerApi.V1.List xs ->
      Aeson.object
        [ "array"
            .= Aeson.object
              [ "items" .= Aeson.Array (datumToJson <$> Vec.fromList xs)
              ]
        ]

serialisePubKeyHash :: PlutusLedgerApi.V1.PubKeyHash -> Hash28
serialisePubKeyHash = MkBlake2b244Hex . serialiseAsHex . PlutusLedgerApi.V1.getPubKeyHash

serialiseCurrencySymbol :: PlutusLedgerApi.V1.CurrencySymbol -> Hash28
serialiseCurrencySymbol = MkBlake2b244Hex . serialiseAsHex . PlutusLedgerApi.V1.unCurrencySymbol

serialiseScriptHash :: PlutusLedgerApi.V1.ScriptHash -> Hash28
serialiseScriptHash = MkBlake2b244Hex . serialiseAsHex . PlutusLedgerApi.V1.getScriptHash

serialiseTxHash :: PlutusLedgerApi.V1.TxId -> Hash32
serialiseTxHash = MkBlake2b255Hex . serialiseAsHex . PlutusLedgerApi.V1.getTxId

serialiseAsHex :: PlutusLedgerApi.V1.BuiltinByteString -> T.Text
serialiseAsHex =
  Base16.Types.extractBase16
    . Base16.encodeBase16
    . PlutusLedgerApi.V1.fromBuiltin

plutusAddressToOuraAddress :: (HasCallStack) => PlutusLedgerApi.V1.Address -> Address
plutusAddressToOuraAddress =
  MkAddressAsBase64
    . Base64.extractBase64
    . Base64.encodeBase64
    . SerialiseRaw.serialiseToRawBytes
    . either error id
    . Address.plutusAddressToShelleyAddress Ledger.Mainnet
