{-# LANGUAGE QualifiedDo #-}

module Plutarch.Extras where

import Prelude

import Plutarch
import Plutarch.Builtin
import Plutarch.LedgerApi
import Plutarch.LedgerApi.Value
import Plutarch.Maybe (pfromJust)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

pMkAdaOnlyValue :: Term s (PInteger :--> PValue Unsorted NonZero)
pMkAdaOnlyValue = phoistAcyclic $ plam $ \lovelaces ->
  pforgetSorted $
    psingletonData # padaSymbolData # pdata padaToken # pdata lovelaces

pscriptHashAddress :: Term s (PAsData PScriptHash :--> PAddress)
pscriptHashAddress = plam $ \datahash ->
  let credential = pcon (PScriptCredential (pdcons @"_0" # datahash #$ pdnil))
      nothing = pdata $ pcon (PDNothing pdnil)
      inner = pdcons @"credential" # pdata credential #$ pdcons @"stakingCredential" # nothing #$ pdnil
   in pcon (PAddress inner)

ppkhAddress :: Term s (PAsData PPubKeyHash :--> PAddress)
ppkhAddress = plam $ \datahash ->
  let credential = pcon (PPubKeyCredential (pdcons @"_0" # datahash #$ pdnil))
      nothing = pdata $ pcon (PDNothing pdnil)
      inner = pdcons @"credential" # pdata credential #$ pdcons @"stakingCredential" # nothing #$ pdnil
   in pcon (PAddress inner)

getOwnAddress :: ClosedTerm (PAsData PScriptContext :--> PAsData PAddress)
getOwnAddress = phoistAcyclic $ plam $ \ctx -> P.do
  PSpending outRef' <- pmatch $ pfromData $ pfield @"purpose" # ctx
  pfield @"address"
    #$ pfield @"resolved"
    #$ pfromJust
    #$ (pfindOwnInput # (pfield @"inputs" #$ pfield @"txInfo" # ctx))
    #$ pfield @"_0"
    # outRef'
