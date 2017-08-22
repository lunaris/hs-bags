{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TypeFamilies               #-}

module Bags
  ( module Exports

  , module Bags
  ) where

import Atomic     as Exports
import Form       as Exports
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

instance Valid Name where
  type Plain Name
    = String
  type PlainError Name
    = String
  validatePlain
    = \case
        "" -> Failure "Empty string"
        s  -> Success (Name s)
  unvalidateValid
    = _nameString

instance FormField Name where
  toFormField result
    = [ TextQ TextQuestion
          { _tqKey      = QuestionKey "Name"
          , _tqQuestion = "What is your name?"
          , _tqValue    = value
          , _tqError    = err
          }

      ]

    where
      (value, err)
        = case result of
            MissingField ->
              (Nothing, Just "Please tell us your name")
            InvalidField (v, e) ->
              (Just (show v), Just e)
            ValidField v ->
              (Just (show (unvalidateValid v)), Nothing)

newtype Age
  = Age { _ageInt :: Int }
  deriving (Show, ToJSON)

instance Valid Age where
  type Plain Age
    = Int
  type PlainError Age
    = String
  validatePlain x
    | x < 0     = Failure "Non-positive age"
    | otherwise = Success (Age x)
  unvalidateValid
    = _ageInt

instance FormField Age where
  toFormField result
    = [ TextQ TextQuestion
          { _tqKey      = QuestionKey "Age"
          , _tqQuestion = "How old are you?"
          , _tqValue    = value
          , _tqError    = err
          }

      ]

    where
      (value, err)
        = case result of
            MissingField ->
              (Nothing, Just "Please tell us your age")
            InvalidField (v, e) ->
              (Just (show v), Just e)
            ValidField v ->
              (Just (show (unvalidateValid v)), Nothing)
