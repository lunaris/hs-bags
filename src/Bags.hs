{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Bags
  ( module Exports

  , module Bags
  ) where

import Atomic     as Exports
import Form       as Exports
import JSON       as Exports
import Operations as Exports
import Paths      as Exports
import Types      as Exports
import Validation as Exports

import Data.Aeson hiding (Result(..))

type PersonFields
  = '[ Key "Name" Name
     , Key "Age"  Age
     , Key "Pet"  PetFields
     ]

type PetFields
  = '[ Key "Name" Name
     , Key "Age"  Age
     ]

type PersonContextuals
  = '[ Key "NameAge" (Either String String)
     ]

data PersonForm
  = PersonForm

instance Form PersonForm where
  type FormFields PersonForm
    = PersonFields
  type FormContextuals PersonForm
    = PersonContextuals

instance FormField PersonForm [p| Name |] [ps| NameAge, NameAge |] where
  toFormField _form result _nameAge1 _nameAge2
    = TextQ TextQuestion
        { _tqKey      = QuestionKey "Name"
        , _tqQuestion = "What is your name?"
        , _tqValue    = value
        , _tqError    = err
        }

    where
      (value, err)
        = case result of
            MissingKey ->
              (Nothing, Just "Please tell us your name")
            InvalidKey (v, e) ->
              (Just (show v), Just e)
            ValidKey v ->
              (Just (show (unvalidateValid v)), Nothing)

instance FormField PersonForm [p| Pet/Name |] [ps| NameAge |] where
  toFormField _form result _nameAge
    = TextQ TextQuestion
        { _tqKey      = QuestionKey "Pet name"
        , _tqQuestion = "What is your pet's name?"
        , _tqValue    = value
        , _tqError    = err
        }

    where
      (value, err)
        = case result of
            MissingKey ->
              (Nothing, Just "Please tell us your pet's name")
            InvalidKey (v, e) ->
              (Just (show v), Just e)
            ValidKey v ->
              (Just (show (unvalidateValid v)), Nothing)

newtype Name
  = Name { _nameString :: String }
  deriving (Eq, Show, ToJSON)

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

newtype Age
  = Age { _ageInt :: Int }
  deriving (Eq, Show, ToJSON)

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

b1, b2, b3, b4
  :: Bag Unvalidated PersonFields

b1 = insertPlain @[p| Name |] "Will" empty
b2 = insertPlain @[p| Age |] 30 b1
b3 = insertPlain @[p| Age |] (-2) b1
b4 = insertPlain @[p| Pet/Name |] "Fido" b2

data Person
  = Person
      { _pName :: Name
      , _pAge  :: Age
      }

  deriving (Eq, Show)

mkPerson :: Name -> Age -> Validation (Either String String) Person
mkPerson n a
  | n == Name "Will" = Failure (Left "Bad name")
  | a == Age 30      = Failure (Right "Bad age")
  | otherwise        = Success (Person n a)

c1 :: Builder PersonFields PersonContextuals Age
c1
  = (   (,,)
    <$> requireValid @[p| Name |]
    <*> requireValid @[p| Age |]
    <*> lookupValid @[p| Pet/Name |]
    )

    `andThen` \(_name, age, petNameResult) ->
      case petNameResult of
        MissingKey ->
          pure age
        InvalidKey _ ->
          pure age
        ValidKey _petName ->
          requireValid @[p| Pet/Age |]
