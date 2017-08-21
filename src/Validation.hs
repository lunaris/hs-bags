{-# LANGUAGE LambdaCase #-}

module Validation
  ( Validation (..)
  ) where

import Data.Semigroup

data Validation e a
  = Failure e
  | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap f
    = \case
        Failure e -> Failure e
        Success x -> Success (f x)

instance Semigroup e => Applicative (Validation e) where
  pure
    = Success

  Failure e1 <*> Failure e2
    = Failure (e1 <> e2)
  Failure e <*> _
    = Failure e
  _ <*> Failure e
    = Failure e
  Success f <*> Success x
    = Success (f x)
