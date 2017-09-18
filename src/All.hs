{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module All
  ( All (..)
  , Unconstrained
  ) where

import HList

import Data.Proxy
import GHC.Exts

class All (c :: k -> Constraint) (as :: [k]) where
  withAll  :: Monoid m => (forall x. c x => Proxy x -> m) -> m
  toHListF :: (forall x. c x => f x) -> HListF f as

instance All c '[] where
  withAll _
    = mempty
  toHListF _
    = HNilF

instance (c a, All c as) => All c (a ': as) where
  withAll k
    = k (Proxy @a) `mappend` withAll @_ @c @as k
  toHListF k
    = HConsF k (toHListF @_ @c k)

class Unconstrained (a :: k)
instance Unconstrained (a :: k)
