{-# LANGUAGE TypeInType #-}

module Types
  ( Bag (..)
  ) where

import Assocs

import           Data.Dynamic
import           Data.Kind
import qualified Data.Map.Strict as M
import qualified Data.Text       as Tx

newtype Bag (f :: Type -> Type) (as :: [Assoc Type])
  = Bag { _bagMap :: M.Map Tx.Text Dynamic }

instance Monoid (Bag f as) where
  mempty
    = Bag mempty
  mappend (Bag m1) (Bag m2)
    = Bag (mappend m1 m2)
