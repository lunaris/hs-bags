{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TypeFamilies               #-}

module Bags
  ( module Exports

  , module Bags
  ) where

import Atomic     as Exports
import JSON       as Exports
import Operations as Exports
import Types      as Exports
import Validation as Exports

import Data.Aeson hiding (Result(..))

type PersonFields
  = '[ Field "Name" Name
     , Field "Age"  Age
     , Field "Pet"  PetFields
     ]

type PetFields
  = '[ Field "Name" Name
     , Field "Age"  Age
     ]

newtype Name
  = Name { _nameString :: String }
  deriving (Show, ToJSON)

instance Atomic Name where
  type Unvalidated Name
    = String
  type ValidationError Name
    = String
  validateAtom
    = \case
        "" -> Failure "Empty string"
        s  -> Success (Name s)
  unvalidateAtom
    = _nameString

newtype Age
  = Age { _ageInt :: Int }
  deriving (Show, ToJSON)

instance Atomic Age where
  type Unvalidated Age
    = Int
  type ValidationError Age
    = String
  validateAtom x
    | x < 0     = Failure "Non-positive age"
    | otherwise = Success (Age x)
  unvalidateAtom
    = _ageInt
