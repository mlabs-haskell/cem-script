module Cardano.CEM.Monads where

import Prelude

import PlutusLedgerApi.V1.Address (Address)

import Cardano.Api hiding (Address)

import Cardano.CEM

-- Common

type Era = BabbageEra

-- MonadBlockchainParams

{- | This monad gives access to all information about Cardano params,
 | which is various kind of Ledger params and ValidityBound/Slots semantics
-}
class (Monad m) => MonadBlockchainParams m where
  askNetworkId :: m NetworkId
  queryCurrentSlot :: m SlotNo

-- MonadQuery

data UtxoQuery
  = ByAddress Address
  | ByTxIns [TxIn]

class (MonadBlockchainParams m) => MonadQueryUtxo m where
  queryUtxo :: UtxoQuery -> m (UTxO Era)
