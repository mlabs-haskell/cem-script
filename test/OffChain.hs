module OffChain where

import Prelude

import Data.Map (keys)

import Cardano.Api hiding (queryUtxo)
import Cardano.Api.Shelley (ReferenceScript (..), toMaryValue)

import PlutusLedgerApi.V2 (always)

import Test.Hspec (describe, it)

import Cardano.CEM.Examples.Compilation ()
import Cardano.CEM.Monads
import Cardano.CEM.OffChain (fromPlutusAddressInMonad)
import Cardano.Extras (
  signingKeyToAddress,
  withKeyWitness,
 )

import Utils (awaitEitherTx, execClb)

execOn = execClb

offChainSpec = describe "Checking monad works" $ do
  it "Asking NetworkId works" $ execOn $ do
    _networkId <- askNetworkId
    return ()
  -- liftIO $ networkId `shouldBe` localDevnetNetworkId
  it "Querying blockchain params works" $ execOn $ do
    _slotNo <- queryCurrentSlot
    _blockchainParams <- queryBlockchainParams
    return ()
  it "Querying UTxO works" $ execOn $ do
    address <- signingKeyToAddress <$> (!! 0) <$> getTestWalletSks
    _utxo <- queryUtxo $ ByAddresses [address]
    return ()
  it "Sending transaction works" $ execOn $ do
    key1 <- (!! 0) <$> getTestWalletSks
    key2 <- (!! 1) <$> getTestWalletSks
    utxo <- queryUtxo $ ByAddresses [signingKeyToAddress key1]
    user1Address <- fromPlutusAddressInMonad $ signingKeyToAddress key1
    user2Address <- fromPlutusAddressInMonad $ signingKeyToAddress key2
    let
      user1TxIns = keys $ unUTxO utxo
      convert x =
        TxOutValueShelleyBased shelleyBasedEra $
          toMaryValue x
      out userAddress =
        TxOut
          userAddress
          ( convert (lovelaceToValue $ fromInteger 3_000_000)
          )
          TxOutDatumNone
          ReferenceScriptNone
      tx =
        MkResolvedTx
          { txIns = map withKeyWitness user1TxIns
          , txInsReference = []
          , txOuts =
              [ out user1Address
              , out user2Address
              ]
          , toMint = TxMintNone
          , interval = always
          , additionalSigners = []
          , signer = key1
          }
    awaitEitherTx =<< submitResolvedTx tx

    return ()
