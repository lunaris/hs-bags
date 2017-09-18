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

type family Uncurry (as :: [*]) (r :: *) :: * where
  Uncurry (a ': as) r
    = a -> Uncurry as r
  Uncurry '[] r
    = r

class Arguments as where
  uncurryHListF :: (HListF f as -> r) -> UncurryF f as r
  curryHListF   :: UncurryF f as r -> HListF f as -> r

instance Arguments '[] where
  uncurryHListF k
    = k HNilF
  curryHListF r HNilF
    = r

instance Arguments as => Arguments (a ': as) where
  uncurryHListF k x
    = uncurryHListF (k . HConsF x)
  curryHListF k (HConsF x xs)
    = curryHListF (k x) xs
