{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Operations where

import Paths
import Types

import           Data.Dynamic
import           Data.Functor.Identity
import qualified Data.Map.Strict       as M
import           Prelude               hiding (lookup)

empty :: Bag f fields
empty
  = Bag M.empty

insert
  :: forall name f fields ty.
     (Typeable f,
      HasField fields name ty)

  => f ty
  -> Bag f fields
  -> Bag f fields

insert x (Bag m)
  = Bag (M.insert (pathText @name) (toDyn x) m)

lookup
  :: forall name f fields ty.
     (Typeable f,
      HasField fields name ty)

  => Bag f fields
  -> Maybe (f ty)

lookup (Bag m)
  = M.lookup (pathText @name) m >>= fromDynamic

insertValue
  :: forall name fields ty.
     HasField fields name ty
  => ty
  -> Bag Identity fields
  -> Bag Identity fields

insertValue
  = insert @name . Identity

lookupValue
  :: forall name fields ty.
     HasField fields name ty
  => Bag Identity fields
  -> Maybe ty

lookupValue
  = fmap runIdentity . lookup @name
