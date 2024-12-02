{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE PolyKinds #-}

module Data.Spine (HasSpine (..), deriveSpine, OfSpine (..)) where

import Prelude

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- | Definitions

{- | Spine is datatype, which tags only constructors of ADT skipping their content.
     TH deriving utility generates Spines which are Enums but one could introduce
     more complex Spine datatypes manually.
-}
class
  ( Ord (Spine sop)
  , Show (Spine sop)
  ) =>
  HasSpine sop
  where
  type Spine sop
  getSpine :: sop -> Spine sop

instance (HasSpine sop1, HasSpine sop2) => HasSpine (sop1, sop2) where
  type Spine (sop1, sop2) = (Spine sop1, Spine sop2)
  getSpine (d1, d2) = (getSpine d1, getSpine d2)

instance (HasSpine sop) => HasSpine (Maybe sop) where
  type Spine (Maybe sop) = Maybe (Spine sop)
  getSpine = fmap getSpine

-- | Newtype encoding sop value of fixed known spine
newtype OfSpine (x :: Spine datatype) = UnsafeMkOfSpine {getValue :: datatype}

-- | Deriving utils
addSuffix :: Name -> String -> Name
addSuffix (Name (OccName name) flavour) suffix =
  Name (OccName $ name <> suffix) flavour

reifyDatatype :: Name -> Q (Name, [Name])
reifyDatatype ty = do
  (TyConI tyCon) <- reify ty
  (name, cs :: [Con]) <-
    case tyCon of
      DataD _ n _ _ cs _ -> pure (n, cs)
      NewtypeD _ n _ _ cs _ -> pure (n, [cs])
      _ -> fail "deriveTags: only 'data' and 'newtype' are supported"
  csNames <- mapM consName cs
  return (name, csNames)

consName :: (MonadFail m) => Con -> m Name
consName cons =
  case cons of
    NormalC n _ -> return n
    RecC n _ -> return n
    _ -> fail "deriveTags: constructor names must be NormalC or RecC (See https://hackage.haskell.org/package/template-haskell-2.20.0.0/docs/src/Language.Haskell.TH.Syntax.html#Con)"

deriveTags :: Name -> String -> [Name] -> Q [Dec]
deriveTags ty suff classes = do
  (tyName, csNames) <- reifyDatatype ty
  -- XXX: Quasi-quote splice does not work for case matches list
  let cs = map (\name -> NormalC (addSuffix name suff) []) csNames
      v =
        DataD [] (addSuffix tyName suff) [] Nothing cs [DerivClause (Just StockStrategy) (ConT <$> classes)]
  pure [v]

deriveMapping :: Name -> String -> Q Exp
deriveMapping ty suff = do
  (_, csNames) <- reifyDatatype ty
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
  spineDec <- deriveTags name suffix [''Eq, ''Ord, ''Enum, ''Show]
  -- TODO: derive Sing
  -- TODO: derive HasField (OfSpine ...)

  decls <-
    [d|
      instance HasSpine $(conT name) where
        type Spine $(conT name) = $(conT spineName)
        getSpine = $(deriveMapping name suffix)
      |]
  return $ spineDec <> decls
