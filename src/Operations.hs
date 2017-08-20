{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Operations where

import Paths
import Types

import           Data.Dynamic
import qualified Data.Map.Strict as M

empty :: Bag fields
empty
  = Bag M.empty

insert
  :: forall name fields ty.
     HasField fields name ty
  => ty
  -> Bag fields
  -> Bag fields

insert x (Bag m)
  = Bag (M.insert (pathText @name) (toDyn x) m)

lookup :: forall name fields ty. HasField fields name ty => Bag fields -> Maybe ty
lookup (Bag m)
  = M.lookup (pathText @name) m >>= fromDynamic
