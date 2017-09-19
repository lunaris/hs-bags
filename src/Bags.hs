{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Bags
  ( module Exports

  , module Bags
  ) where

import Assocs     as Exports
import Atomic     as Exports
import Form       as Exports
import JSON       as Exports
import Operations as Exports
import Types      as Exports
import Validation as Exports

import Data.Aeson hiding (Result(..))

type PersonFields
  = '[ "Name"     :-> Name
     , "Age"      :-> Age
     , "Pet/Name" :-> Name
     , "Pet/Age"  :-> Age
     ]

type PersonContextuals
  = '[ '("NameAge", Either String String)
     ]

type (:->) = '(,)

type PersonGroups
  = '[ "PersonalDetails" :->
       '[ "Name"
        , "Age"
        ]

     , "PetDetails" :->
        '[ "Pet/Name"
         , "Pet/Age"
         ]

     ]

data PersonForm
  = PersonForm

instance Form PersonForm where
  type FormFields PersonForm
    = PersonFields
  type FormContextuals PersonForm
    = PersonContextuals
  type FormGroups PersonForm
    = PersonGroups

instance FormField PersonForm "Name" where
  type ContextualKeys PersonForm "Name"
    = '["NameAge", "NameAge"]
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

instance FormField PersonForm "Age" where
  type ContextualKeys PersonForm "Age"
    = '[]
  toFormField _form result
    = TextQ TextQuestion
        { _tqKey      = QuestionKey "Age"
        , _tqQuestion = "What is your age?"
        , _tqValue    = value
        , _tqError    = err
        }

    where
      (value, err)
        = case result of
            MissingKey ->
              (Nothing, Just "Please tell us your age")
            InvalidKey (v, e) ->
              (Just (show v), Just e)
            ValidKey v ->
              (Just (show (unvalidateValid v)), Nothing)

instance FormField PersonForm "Pet/Name" where
  type ContextualKeys PersonForm "Pet/Name"
    = '["NameAge"]
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

instance FormField PersonForm "Pet/Age" where
  type ContextualKeys PersonForm "Pet/Age"
    = '[]
  toFormField _form result
    = TextQ TextQuestion
        { _tqKey      = QuestionKey "Pet age"
        , _tqQuestion = "What is your pet's age?"
        , _tqValue    = value
        , _tqError    = err
        }

    where
      (value, err)
        = case result of
            MissingKey ->
              (Nothing, Just "Please tell us your pet's age")
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

b1, b2, b3, b4, b5
  :: Bag Unvalidated PersonFields

b1 = insertPlain @"Name" "Will" mempty
b2 = insertPlain @"Age" 30 b1
b3 = insertPlain @"Age" (-2) b1
b4 = insertPlain @"Pet/Name" "Fido" b2
b5 = insertPlain @"Name" "Matt" $ insertPlain @"Age" 28 b2

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

c1 :: Builder PersonFields PersonContextuals Person
c1
  = (   (,)
    <$> requireValid @"Name"
    <*> requireValid @"Age"
    )

    `andThen` \(name, age) ->
      validateContextual @"NameAge" (mkPerson name age)

c2 :: Builder PersonFields PersonContextuals (Person, Name, Age)
c2
  =   (,,)
  <$> c1
  <*> requireValid @"Pet/Name"
  <*> requireValid @"Pet/Age"
