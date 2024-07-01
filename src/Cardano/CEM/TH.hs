module Cardano.CEM.TH (
  deriveCEMAssociatedTypes,
  resolveFamily,
  compileCEM,
  unstableMakeIsDataSchema,
  defaultIndex,
  unstableMakeHasSchemaInstance,
) where

import Prelude

import Data.Data (Proxy (..))
import Data.Map qualified as Map
import Data.Tuple (swap)
import GHC.Num.Natural (Natural)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (sequenceQ)

import PlutusTx qualified
import PlutusTx.Blueprint.TH
import PlutusTx.IsData (unsafeFromBuiltinData)
import PlutusTx.Show (deriveShow)

import Language.Haskell.TH.Datatype (
  ConstructorInfo (..),
  DatatypeInfo (..),
  reifyDatatype,
 )
import Plutarch (Config (..), LogLevel (..), TracingMode (..), compile)

import Cardano.CEM (CEMScript (..), CEMScriptTypes (..), CompilationConfig (..))
import Cardano.CEM.DSL
import Cardano.CEM.OnChain (CEMScriptCompiled (..), genericPlutarchScript)
import Data.Spine (derivePlutusSpine)

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

deriveCEMAssociatedTypes :: Bool -> Name -> Q [Dec]
deriveCEMAssociatedTypes _deriveBlueprint scriptName = do
  declss <-
    sequenceQ
      [ -- Data and Spines
        deriveFamily derivePlutusSpine ''State
      , deriveFamily derivePlutusSpine ''Transition
      , deriveFamily derivePlutusSpine ''Params
      , -- Other
        deriveShow scriptName
      ]
  return $ concat declss
  where
    deriveFamily deriver family = do
      name <- resolveFamily family scriptName
      deriver name

compileCEM :: Bool -> Name -> Q [Dec]
compileCEM debugBuild name = do
  -- FIXIT: two duplicating cases on `transitionComp`
  let plutusScript =
        [|
          \a b c -> case transitionComp @($(conT name)) of
            Just f ->
              PlutusTx.toBuiltinData $
                f
                  (unsafeFromBuiltinData a)
                  (unsafeFromBuiltinData b)
                  (unsafeFromBuiltinData c)
            Nothing -> PlutusTx.toBuiltinData ()
          |]

  compiledName <- newName $ "compiled_" <> nameBase name

  [d|
    $(varP compiledName) = case compiled of
      Right x -> (errorCodes', x)
      Left message ->
        error $ "Plutarch compilation error: " <> show message
      where
        mPlutusScript = case transitionComp @($(conT name)) of
          Just _ -> Just $(PlutusTx.compileUntyped plutusScript)
          Nothing -> Nothing
        spec' =
          preProcessForOnChainCompilation $
            perTransitionScriptSpec @($(conT name))
        MkCompilationConfig prefix = compilationConfig @($(conT name))
        errorCodes' = parseErrorCodes prefix spec'
        spec =
          if debugBuild
            then spec'
            else substErrors (Map.fromList $ map swap errorCodes') spec'
        script =
          genericPlutarchScript
            @($(conT name))
            spec
            mPlutusScript
        plutarchConfig =
          if debugBuild
            then Tracing LogDebug DoTracing
            else Tracing LogInfo DoTracing
        compiled = compile plutarchConfig script

    instance CEMScriptCompiled $(conT name) where
      errorCodes Proxy = fst $(varE compiledName)
      cemScriptCompiled Proxy = snd $(varE compiledName)
    |]
