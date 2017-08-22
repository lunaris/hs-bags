{-# LANGUAGE LambdaCase #-}

module Validation
  ( Validation (..)
  , validating
  ) where

import Data.Semigroup

data Validation e a
  = Failure e
  | Success a
  deriving (Eq, Show)

validating :: (e -> b) -> (a -> b) -> Validation e a -> b
validating f g
  = \case
      Failure e -> f e
      Success x -> g x

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
