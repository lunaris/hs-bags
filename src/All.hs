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
  ) where

import Data.Proxy
import GHC.Exts

class All (c :: k -> Constraint) (as :: [k]) where
  withAll :: (forall x. c x => Proxy x -> r) -> [r]

instance All c '[] where
  withAll _ = []

instance (c a, All c as) => All c (a ': as) where
  withAll k = k (Proxy @a) : withAll @_ @c @as k
