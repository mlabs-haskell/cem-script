module Cardano.CEM.Constraints where

import Prelude


import PlutusTx.IsData (toData)
import PlutusTx.Prelude
import Prelude (Show)
import Prelude qualified

import Data.Data (Proxy)
import Data.Map qualified as Map

import PlutusLedgerApi.V1.Address (Address, pubKeyHashAddress)
import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V2 (
  BuiltinData (..),
  Data (..),
  FromData (..),
  ToData (..),
  Value,
  fromData,
 )
import PlutusTx.Show.TH (deriveShow)

import Cardano.CEM.Stages ( Stages(..) )


-- | This is different ways to specify address
data AddressSpec
  = ByAddress Address
  | ByPubKey PubKeyHash
  | ByScript -- TODO
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
  | -- TODO
    BySameCEM BuiltinData
  | ByDatum BuiltinData
  deriving stock (Show, Prelude.Eq)

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
  deriving (Show)

-- TH deriving done at end of file for GHC staging reasons

deriveShow ''TxFanKind
deriveShow ''TxFanFilter'
