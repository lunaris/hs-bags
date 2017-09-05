{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TypeOperators  #-}

module HList where

data HListF :: (k -> *) -> [k] -> * where
  HNilF  :: HListF f '[]
  HConsF :: f a -> HListF f as -> HListF f (a ': as)

type family Append (as :: [k]) (bs :: [k]) where
  Append '[]       bs = bs
  Append (a ': as) bs = a ': Append as bs

appendF :: HListF f as -> HListF f bs -> HListF f (Append as bs)
appendF xs ys
  = case xs of
      HNilF        -> ys
      HConsF x xs' -> HConsF x (appendF xs' ys)

type family UncurryF (f :: k -> *) (as :: [k]) (r :: *) :: * where
  UncurryF f (a ': as) r
    = f a -> UncurryF f as r
  UncurryF f '[] r
    = r

class Curried as where
  uncurryHListF :: (HListF f as -> r) -> UncurryF f as r

instance Curried '[] where
  uncurryHListF k
    = k HNilF

instance Curried as => Curried (a ': as) where
  uncurryHListF k x
    = uncurryHListF (k . HConsF x)
