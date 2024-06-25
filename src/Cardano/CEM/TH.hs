module Cardano.CEM.TH (
  deriveCEMAssociatedTypes,
  resolveFamily,
  compileCEM,
  unstableMakeIsDataSchema,
  deriveStageAssociatedTypes,
  defaultIndex,
  unstableMakeHasSchemaInstance,
) where

import Prelude

import Data.Data (Proxy (..))
import GHC.Num.Natural (Natural)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (sequenceQ)
import Control.Monad.Trans (MonadIO (..))
import Debug.Trace (trace)

import PlutusTx qualified
import PlutusTx.Blueprint.TH
import PlutusTx.Show (deriveShow)

import Language.Haskell.TH.Datatype (
  ConstructorInfo (..),
  DatatypeInfo (..),
  reifyDatatype,
 )
import Plutarch (Config (..), LogLevel (..), TracingMode (..), compile, prettyScript)
import Plutarch.Script (serialiseScript)
import Prettyprinter (pretty)

import Cardano.CEM (CEMScriptTypes (..))
import Cardano.CEM.OnChain (CEMScriptCompiled (..), genericPlutarchScript)
import Cardano.CEM.Stages (Stages (..))
import Data.Spine (deriveSpine)

defaultIndex :: Name -> Q [(Name, Natural)]
defaultIndex name = do
  info <- reifyDatatype name
  pure $ zip (constructorName <$> datatypeCons info) [0 ..]

unstableMakeIsDataSchema :: Name -> Q [InstanceDec]
unstableMakeIsDataSchema name = do
  index <- defaultIndex name
  PlutusTx.makeIsDataSchemaIndexed name index

unstableMakeHasSchemaInstance :: Name -> Q [InstanceDec]
unstableMakeHasSchemaInstance name = do
  index <- defaultIndex name
  dec <- makeHasSchemaInstance name index
  return [dec]

-- | Get `TypeFamily Datatype` result as TH Name
resolveFamily :: Name -> Name -> Q Name
resolveFamily familyName argName = do
  argType <- conT argName
  [TySynInstD (TySynEqn _ _ (ConT name))] <-
    reifyInstances familyName [argType]
  return name

deriveStageAssociatedTypes :: Name -> Q [Dec]
deriveStageAssociatedTypes stageName = do
  stageParamsName <- resolveFamily ''StageParams stageName
  declss <-
    sequenceQ
      [ PlutusTx.unstableMakeIsData stageName
      , PlutusTx.unstableMakeIsData stageParamsName
      ]
  return $ concat declss

deriveCEMAssociatedTypes :: Bool -> Name -> Q [Dec]
deriveCEMAssociatedTypes deriveBlueprint scriptName = do
  declss <-
    sequenceQ
      [ -- Data
        deriveFamily isDataDeriver ''Params
      , deriveFamily isDataDeriver ''State
      , deriveFamily isDataDeriver ''Transition
      , -- Spines
        deriveFamily deriveSpine ''State
      , deriveFamily deriveSpine ''Transition
      , deriveFamily deriveSpine ''Params
      , -- Other
        deriveShow scriptName
      ]
  return $ concat declss
  where
    isDataDeriver =
      if deriveBlueprint
        then unstableMakeIsDataSchema
        else PlutusTx.unstableMakeIsData
    deriveFamily deriver family = do
      name <- resolveFamily family scriptName
      deriver name

compileCEM :: Name -> Q [Dec]
compileCEM name = do
  stageName <- resolveFamily ''Stage name
  let compiled =
        [|compile (Tracing LogDebug DoTracing) $(genericPlutarchScript name stageName)|]
  let script' = [| case $compiled of Right script -> script |]
  [d|
    instance CEMScriptCompiled $(conT name) where
      {-# INLINEABLE cemScriptCompiled #-}
      cemScriptCompiled Proxy = script
        where
          script = $script'
    |]
