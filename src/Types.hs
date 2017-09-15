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
  , Key

  , HasKeys' (..)
  , HasKeys

  , HasKey' (..)
  , HasKey

  , KeyAssocs
  ) where

import All
import Paths

import           Data.Dynamic
import qualified Data.Map.Strict           as M
import qualified Data.Text                 as Tx
import           GHC.Exts
import           GHC.TypeLits

newtype Bag f (keys :: [*])
  = Bag { _bagMap :: M.Map Tx.Text Dynamic }

data Key (name :: Symbol) ty

class HasKeys' (keys :: [*]) (paths :: [Path]) where
  type KeysTypes keys paths :: [*]

instance (All Typeable tys,
          LookupAll keys paths ~ maybeTys,
          maybeTys ~ 'Just tys,
          ErrorWhenNoTypes keys paths maybeTys)

      =>  HasKeys' (keys :: [*]) (paths :: [Path]) where

  type KeysTypes keys paths
    = FromJust (LookupAll keys paths)

class HasKeys (keys :: [*]) (paths :: [Path]) (tys :: [*]) | keys paths -> tys

instance (HasKeys' keys paths,
          KeysTypes keys paths ~ tys)

      =>  HasKeys (keys :: [*]) (paths :: [Path]) (tys :: [*])

type family LookupAll (keys :: [*]) (paths :: [Path]) :: Maybe [*] where
  LookupAll keys (path ': paths)
    = LiftConsMaybe (Lookup keys path) (LookupAll keys paths)
  LookupAll keys '[]
    = 'Just '[]

type family LiftConsMaybe (a :: Maybe k) (as :: Maybe [k]) :: Maybe [k] where
  LiftConsMaybe ('Just a) ('Just as)
    = 'Just (a ': as)
  LiftConsMaybe _ _
    = 'Nothing

type family ErrorWhenNoTypes keys (paths :: [Path]) (maybeTys :: Maybe [*]) :: Constraint where
  ErrorWhenNoTypes _ _ ('Just _)
    = ()
  ErrorWhenNoTypes keys paths 'Nothing
    = TypeError
        (     'Text "The keys: "
        ':$$: PathsError ('Text "  ") paths
        ':$$: 'Text " could not be found. The available keys are:"
        ':$$: KeyNamesError ('Text "  ") keys
        )

type family PathsError (prefix :: ErrorMessage) (paths :: [Path]) :: ErrorMessage where
  PathsError prefix '[path]
    =     prefix
    ':<>: PathError path
  PathsError prefix (path ': paths)
    =     prefix
    ':<>: PathError path
    ':$$: PathsError prefix paths

class (All KnownSymbol (PathNames path),
       Typeable (KeyType keys path))

    => HasKey' (keys :: [*]) (path :: Path) where

  type KeyType keys path :: *

instance (All KnownSymbol (PathNames path),
          Typeable ty,
          Lookup keys path ~ maybeTy,
          maybeTy ~ 'Just ty,
          ErrorWhenNoType keys path maybeTy)

      =>  HasKey' (keys :: [*]) (path :: Path) where

  type KeyType keys path
    = FromJust (Lookup keys path)

class (All KnownSymbol (PathNames path), Typeable ty)
    => HasKey (keys :: [*]) (path :: Path) (ty :: *) | keys path -> ty

instance (All KnownSymbol (PathNames path),
          Typeable ty,
          HasKey' keys path,
          KeyType keys path ~ ty)

      =>  HasKey (keys :: [*]) (path :: Path) (ty :: *)

type family Lookup (keys :: [*]) (path :: k) :: Maybe * where
  Lookup (Key name keys ': _) ('Node name path)
    = Lookup keys path
  Lookup (Key name ty ': _) ('Leaf name)
    = 'Just ty
  Lookup (Key name ty ': _) name
    = 'Just ty
  Lookup (_ ': keys) path
    = Lookup keys path
  Lookup '[] _
    = 'Nothing

type family ErrorWhenNoType keys (path :: Path) (maybeTy :: Maybe *) :: Constraint where
  ErrorWhenNoType _ _ ('Just _)
    = ()
  ErrorWhenNoType keys path 'Nothing
    = TypeError
        (     'Text "The key "
        ':<>: PathError path
        ':<>: 'Text " could not be found. The available keys are:"
        ':$$: KeyNamesError ('Text "  ") keys
        )

type PathError path
  = PathNamesError (PathNames path)

type family PathNamesError (names :: [Symbol]) :: ErrorMessage where
  PathNamesError '[name]
    = 'ShowType name
  PathNamesError (name ': names)
    =     'ShowType name
    ':<>: 'Text " // "
    ':<>: PathNamesError names
  PathNamesError '[]
    = 'Text ""

type family KeyNamesError (prefix :: ErrorMessage) (keys :: [*]) :: ErrorMessage where
  KeyNamesError prefix '[Key name subkeys]
    =     prefix
    ':<>: 'ShowType name
    ':<>: 'Text " //"
    ':$$: KeyNamesError (prefix ':<>: 'Text "  ") subkeys
  KeyNamesError prefix '[Key name ty]
    =     prefix
    ':<>: 'ShowType name
    ':<>: 'Text " :: "
    ':<>: 'ShowType ty
  KeyNamesError prefix (Key name subkeys ': keys)
    =     prefix
    ':<>: 'ShowType name
    ':<>: 'Text " //"
    ':$$: KeyNamesError (prefix ':<>: 'Text "  ") subkeys
    ':$$: KeyNamesError prefix keys
  KeyNamesError prefix (Key name ty ': keys)
    =     prefix
    ':<>: 'ShowType name
    ':<>: 'Text " :: "
    ':<>: 'ShowType ty
    ':$$: KeyNamesError prefix keys

type family FromJust (a :: Maybe k) :: k where
  FromJust ('Just a)
    = a

type family KeyAssocs (f :: * -> *) (keys :: [*]) :: [(Path, *)] where
  KeyAssocs f keys
    = KeyAssocs' f '[] keys

type family KeyAssocs' (f :: * -> *) (path :: [Symbol]) (keys :: [*]) :: [(Path, *)] where
  KeyAssocs' f path (Key name subkeys ': keys)
    =  KeyAssocs' f (name ': path) subkeys
    ++ KeyAssocs' f path keys
  KeyAssocs' f path (Key name ty ': keys)
    =  '(NamesPath (Reverse (name ': path)), f ty)
    ': KeyAssocs' f path keys
  KeyAssocs' _ _ '[]
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
