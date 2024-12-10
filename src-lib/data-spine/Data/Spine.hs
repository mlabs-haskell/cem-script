{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE PolyKinds #-}

{- |
Note about design decision on nested spines.
`getSpine (Just Value) = JustSpine ValueSpine` - looks more usable,
than `getSpine (Just Value) = JustSpine`.
But it seem to break deriving for parametised types like `Maybe a`,
and can be done with `fmap getSpine mValue`. Probably it actually
works exaclty for functorial parameters.
-}
module Data.Spine where

import Prelude

import Data.Data (Proxy)
import Data.List (elemIndex)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import GHC.Natural (Natural)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import PlutusTx (FromData, ToData, UnsafeFromData, unstableMakeIsData)

-- | Definitions

{- | Spine is datatype, which tags only constructors of ADT skipping their content.
     TH deriving utility generates Spines which are Enums but one could introduce
     more complex Spine datatypes manually.
-}
class
  ( Ord (Spine sop)
  , Show (Spine sop)
  , Enum (Spine sop)
  , Bounded (Spine sop)
  ) =>
  HasSpine sop
  where
  type Spine sop = spine | spine -> sop
  getSpine :: sop -> Spine sop

-- | Version of `HasSpine` knowing its Plutus Data encoding
class
  ( HasSpine sop
  , UnsafeFromData sop
  , ToData sop
  , FromData sop
  ) =>
  HasPlutusSpine sop
  where
  fieldsMap :: Map.Map (Spine sop) [String]

toNat :: Int -> Natural
toNat = fromInteger . toInteger

spineFieldsNum :: forall sop. (HasPlutusSpine sop) => Spine sop -> Natural
spineFieldsNum spine =
  toNat $ length $ (fieldsMap @sop) Map.! spine

-- FIXME: use spine do discriminate
fieldNum ::
  forall sop label.
  (HasPlutusSpine sop, KnownSymbol label) =>
  Proxy label ->
  Natural
fieldNum proxyLabel =
  head $ mapMaybe fieldIndex x
  where
    x = Map.elems $ fieldsMap @sop
    fieldName = symbolVal proxyLabel
    fieldIndex dict = toNat <$> elemIndex fieldName dict

allSpines :: forall sop. (HasPlutusSpine sop) => [Spine sop]
allSpines = [Prelude.minBound .. Prelude.maxBound]

-- | Phantom type param is required for `HasSpine` injectivity
data MaybeSpine a = JustSpine | NothingSpine
  deriving stock (Eq, Ord, Show, Bounded, Enum)

-- FIXME: could such types be derived?
instance HasSpine (Maybe x) where
  type Spine (Maybe x) = MaybeSpine x
  getSpine Just {} = JustSpine
  getSpine Nothing = NothingSpine

-- Deriving utils

addSuffix :: Name -> String -> Name
addSuffix (Name (OccName name) flavour) suffix =
  Name (OccName $ name <> suffix) flavour

-- FIXME: cleaner return type
reifyDatatype :: Name -> Q (Name, [Name], [[Name]])
reifyDatatype ty = do
  (TyConI tyCon) <- reify ty
  (name, cs :: [Con]) <-
    case tyCon of
      DataD _ n _ _ cs _ -> pure (n, cs)
      NewtypeD _ n _ _ cs _ -> pure (n, [cs])
      _ -> fail "deriveTags: only 'data' and 'newtype' are supported"
  csNames <- mapM consName cs
  csFields <- mapM consFields cs
  return (name, csNames, csFields)
  where
    fieldName (name, _, _) = name
    consFields (RecC _ fields) = return $ map fieldName fields
    consFields (NormalC _ fields) | length fields == 0 = return []
    consFields _ =
      fail $
        "Spine: only Sum-of-Products are supported, but "
          <> show ty
          <> " is not"

consName :: (MonadFail m) => Con -> m Name
consName cons =
  case cons of
    NormalC n _ -> return n
    RecC n _ -> return n
    _ -> fail "deriveTags: constructor names must be NormalC or RecC (See https://hackage.haskell.org/package/template-haskell-2.20.0.0/docs/src/Language.Haskell.TH.Syntax.html#Con)"

deriveTags :: Name -> String -> [Name] -> Q [Dec]
deriveTags ty suff classes = do
  (tyName, csNames, _) <- reifyDatatype ty
  -- XXX: Quasi-quote splice does not work for case matches list
  let cs = map (\name -> NormalC (addSuffix name suff) []) csNames
      v =
        DataD [] (addSuffix tyName suff) [] Nothing cs [DerivClause (Just StockStrategy) (ConT <$> classes)]
  pure [v]

deriveMapping :: Name -> String -> Q Exp
deriveMapping ty suff = do
  (_, csNames, _) <- reifyDatatype ty
  -- XXX: Quasi-quote splice does not work for case matches list
  let
    matches =
      map
        (\name -> Match (RecP name []) (NormalB (ConE (addSuffix name suff))) [])
        csNames
  return $ LamCaseE matches

{- | Derives `HasSpine`
| Usage: `$(deriveSpine ''HydraEvent)`
-}
deriveSpine :: Name -> Q [Dec]
deriveSpine name = do
  let
    suffix = "Spine"
    spineName = addSuffix name suffix
  spineDec <- deriveTags name suffix [''Eq, ''Ord, ''Enum, ''Show, ''Bounded]

  decls <-
    [d|
      instance HasSpine $(conT name) where
        type Spine $(conT name) = $(conT spineName)
        getSpine = $(deriveMapping name suffix)
      |]
  return $ spineDec <> decls

derivePlutusSpine :: Name -> Q [Dec]
derivePlutusSpine name = do
  decls <- deriveSpine name
  isDataDecls <- unstableMakeIsData name

  (_, _, fieldsNames') <- reifyDatatype name
  let fieldsNames = map (map nameBase) fieldsNames'
  instanceDecls <-
    [d|
      instance HasPlutusSpine $(conT name) where
        fieldsMap =
          Map.fromList $ zip (allSpines @($(conT name))) fieldsNames
      |]

  return $ decls <> isDataDecls <> instanceDecls
