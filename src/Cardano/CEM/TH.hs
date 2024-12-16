module Cardano.CEM.TH (
  deriveCEMAssociatedTypes,
  compileCEMOnchain,
) where

import Cardano.CEM.Compile (preProcessForOnChainCompilation)
import Cardano.CEM.DSL (
  CEMScript (..),
  CEMScriptTypes (..),
  CompilationConfig (..),
  parseErrorCodes,
  substErrors,
 )
import Cardano.CEM.OnChain (CEMScriptCompiled (..), genericPlutarchScript)
import Data.Data (Proxy (..))
import Data.Map qualified as Map
import Data.Spine (derivePlutusSpine)
import Data.Tuple (swap)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (sequenceQ)
import Plutarch (Config (..), LogLevel (..), TracingMode (..), compile)
import PlutusTx qualified
import PlutusTx.IsData (unsafeFromBuiltinData)
import PlutusTx.Show (deriveShow)
import Prelude

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

    -- \| Get `TypeFamily Datatype` result as TH Name
    resolveFamily :: Name -> Name -> Q Name
    resolveFamily familyName argName = do
      argType <- conT argName
      [TySynInstD (TySynEqn _ _ (ConT name))] <-
        reifyInstances familyName [argType]
      return name

compileCEMOnchain :: Bool -> Name -> Q [Dec]
compileCEMOnchain debugBuild name = do
  -- TODO: two duplicating cases on `transitionComp`
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
            transitionSpec @($(conT name))
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
