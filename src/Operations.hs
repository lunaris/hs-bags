{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Operations where

import All
import Assocs
import HList
import Types

import           Control.Monad.Reader
import           Data.Coerce
import           Data.Dynamic
import           Data.Functor.Identity
import qualified Data.Map.Strict       as M
import           Prelude               hiding (lookup)
import           Unsafe.Coerce

empty :: Bag f as
empty
  = Bag M.empty

insert
  :: forall k f as ty.
     (Typeable f,
      HasKey as k ty)

  => f ty
  -> Bag f as
  -> Bag f as

insert x (Bag m)
  = Bag (M.insert (keyText @k) (toDyn x) m)

lookup
  :: forall k f as ty m.
     (MonadReader (Bag f as) m,
      Typeable f,
      HasKey as k ty)

  => m (Maybe (f ty))

lookup
  = asks k
  where
    k (Bag m)
      = M.lookup (keyText @k) m >>= fromDynamic

lookupAll
  :: forall ks f as m.
     (MonadReader (Bag f as) m,
      Typeable f,
      HasKeys' as ks)

  => m (HListF Maybe (KeysValues as ks))

lookupAll
  = asks k
  where
    k :: Bag f as -> HListF Maybe tys
    k b
      = unsafeCoerce @(HListF (MaybeValue f as) ks) (toHListF @_ @(HasKey' as) h)
      where
        h :: forall k. HasKey' as k => MaybeValue f as k
        h
          = MaybeValue (lookup @k b)

newtype MaybeValue f as k
  = MaybeValue { getMaybeValue :: Maybe (f (KeyValue as k)) }

insertValue
  :: forall k as ty.
     HasKey as k ty
  => ty
  -> Bag Identity as
  -> Bag Identity as

insertValue
  = insert @k . Identity

lookupValue
  :: forall k as ty m.
     (MonadReader (Bag Identity as) m,
      HasKey as k ty)

  => m (Maybe ty)

lookupValue
  = coerce <$> lookup @k

union :: Bag f as -> Bag f as -> Bag f as
union (Bag m1) (Bag m2)
  = Bag (m1 `M.union` m2)
