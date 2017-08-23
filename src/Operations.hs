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
  :: forall name f fields ty m.
     (MonadReader (Bag f fields) m,
      Typeable f,
      HasField fields name ty)

  => m (Maybe (f ty))

lookup
  = asks k
  where
    k (Bag m)
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
  :: forall name fields ty m.
     (MonadReader (Bag Identity fields) m,
      HasField fields name ty)

  => m (Maybe ty)

lookupValue
  = coerce <$> lookup @name
