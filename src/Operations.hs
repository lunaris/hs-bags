{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Operations where

import Paths
import Types

import           Control.Monad.Reader
import           Data.Coerce
import           Data.Dynamic
import           Data.Functor.Identity
import qualified Data.Map.Strict       as M
import           Prelude               hiding (lookup)

empty :: Bag f keys
empty
  = Bag M.empty

insert
  :: forall name f keys ty.
     (Typeable f,
      HasKey keys name ty)

  => f ty
  -> Bag f keys
  -> Bag f keys

insert x (Bag m)
  = Bag (M.insert (pathText @name) (toDyn x) m)

lookup
  :: forall name f keys ty m.
     (MonadReader (Bag f keys) m,
      Typeable f,
      HasKey keys name ty)

  => m (Maybe (f ty))

lookup
  = asks k
  where
    k (Bag m)
      = M.lookup (pathText @name) m >>= fromDynamic

insertValue
  :: forall name keys ty.
     HasKey keys name ty
  => ty
  -> Bag Identity keys
  -> Bag Identity keys

insertValue
  = insert @name . Identity

lookupValue
  :: forall name keys ty m.
     (MonadReader (Bag Identity keys) m,
      HasKey keys name ty)

  => m (Maybe ty)

lookupValue
  = coerce <$> lookup @name

union :: Bag f keys -> Bag f keys -> Bag f keys
union (Bag m1) (Bag m2)
  = Bag (m1 `M.union` m2)
