{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Types
  ( Bag (..)
  , Field
  , HasField

  , FieldAssocs
  ) where

import All
import Paths

import           Data.Dynamic
import qualified Data.Map.Strict as M
import qualified Data.Text       as Tx
import           GHC.Exts
import           GHC.TypeLits

newtype Bag f (fields :: [*])
  = Bag { _bagMap :: M.Map Tx.Text Dynamic }

data Field (name :: Symbol) ty

class (All KnownSymbol (PathNames path), Typeable ty)
  =>  HasField (fields :: [*]) (path :: k) (ty :: *) | fields path -> ty where

instance (All KnownSymbol (PathNames path),
          Typeable ty,
          Lookup fields path ~ maybeTy,
          maybeTy ~ 'Just ty,
          ErrorWhenNothing fields path maybeTy)

      =>  HasField (fields :: [*]) (path :: k) (ty :: *) where

type family Lookup (fields :: [*]) (path :: k) :: Maybe * where
  Lookup (Field name fields ': _) ('Node name path)
    = Lookup fields path
  Lookup (Field name ty ': _) ('Leaf name)
    = 'Just ty
  Lookup (Field name ty ': _) name
    = 'Just ty
  Lookup (_ ': fields) path
    = Lookup fields path
  Lookup '[] _
    = 'Nothing

type family ErrorWhenNothing fields (path :: k) (maybeTy :: Maybe *) :: Constraint where
  ErrorWhenNothing _ _ ('Just _)
    = ()
  ErrorWhenNothing fields path 'Nothing
    = TypeError
        (     'Text "The field "
        ':<>: PathNamesError (PathNames path)
        ':<>: 'Text " could not be found. The available fields are:"
        ':$$: FieldNamesError ('Text "  ") fields
        )

type family PathNamesError (names :: [Symbol]) :: ErrorMessage where
  PathNamesError '[name]
    = 'ShowType name
  PathNamesError (name ': names)
    =     'ShowType name
    ':<>: 'Text " // "
    ':<>: PathNamesError names
  PathNamesError '[]
    = 'Text ""

type family FieldNamesError (prefix :: ErrorMessage) (fields :: [*]) :: ErrorMessage where
  FieldNamesError prefix '[Field name subfields]
    =     prefix
    ':<>: 'ShowType name
    ':<>: 'Text " //"
    ':$$: FieldNamesError (prefix ':<>: 'Text "  ") subfields
  FieldNamesError prefix '[Field name ty]
    =     prefix
    ':<>: 'ShowType name
    ':<>: 'Text " :: "
    ':<>: 'ShowType ty
  FieldNamesError prefix (Field name subfields ': fields)
    =     prefix
    ':<>: 'ShowType name
    ':<>: 'Text " //"
    ':$$: FieldNamesError (prefix ':<>: 'Text "  ") subfields
    ':$$: FieldNamesError prefix fields
  FieldNamesError prefix (Field name ty ': fields)
    =     prefix
    ':<>: 'ShowType name
    ':<>: 'Text " :: "
    ':<>: 'ShowType ty
    ':$$: FieldNamesError prefix fields

type family FieldAssocs (f :: * -> *) (fields :: [*]) :: [(Path Symbol, *)] where
  FieldAssocs f fields
    = FieldAssocs' f '[] fields

type family FieldAssocs' (f :: * -> *) (path :: [Symbol]) (fields :: [*]) :: [(Path Symbol, *)] where
  FieldAssocs' f path (Field name subfields ': fields)
    =  FieldAssocs' f (name ': path) subfields
    ++ FieldAssocs' f path fields
  FieldAssocs' f path (Field name ty ': fields)
    =  '(NamesPath (Reverse (name ': path)), f ty)
    ': FieldAssocs' f path fields
  FieldAssocs' _ _ '[]
    = '[]

type family Reverse (as :: [k]) :: [k] where
  Reverse as
    = Reverse' '[] as

type family Reverse' (acc :: [k]) (as :: [k]) :: [k] where
  Reverse' acc '[]
    = acc
  Reverse' acc (a ': as)
    = Reverse' (a ': acc) as

type family (++) (as :: [k]) (bs :: [k]) :: [k] where
  '[]       ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)
