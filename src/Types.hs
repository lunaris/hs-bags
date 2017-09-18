{-# LANGUAGE TypeInType #-}

module Types
  ( Bag (..)
  ) where

import Assocs

import           Data.Dynamic
import           Data.Kind
import qualified Data.Map.Strict as M
import qualified Data.Text       as Tx

newtype Bag f (as :: [Assoc Type])
  = Bag { _bagMap :: M.Map Tx.Text Dynamic }
