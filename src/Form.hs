{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}

module Form
  ( Form (..)
  , FormField (..)

  , QuestionKey (..)

  , Question (..)
  , GBPQuestion (..)
  , TextQuestion (..)

  , printQuestions
  , renderQuestion
  ) where

import Assocs
import Atomic
import HList

import Data.Kind

class Form form where
  type FormFields form      :: [Assoc Type]
  type FormContextuals form :: [Assoc Type]

class (Form form,
       HasKey' (FormFields form) fk,
       HasKeys' (FormContextuals form) cks)

    => FormField form fk cks where

  toFormField
    :: form
    -> ValidLookupResult (KeyValue (FormFields form) fk)
    -> UncurryF Maybe (KeysValues (FormContextuals form) cks) Question

newtype QuestionKey
  = QuestionKey { _questionKeyString :: String }
  deriving (Eq, Show)

data Question
  = GBPQ GBPQuestion
  | TextQ TextQuestion
  deriving (Eq, Show)

data GBPQuestion
  = GBPQuestion
      { _gbpqKey      :: QuestionKey
      , _gbpqQuestion :: String
      , _gbpqValue    :: Maybe String
      , _gbpqError    :: Maybe String
      }

  deriving (Eq, Show)

data TextQuestion
  = TextQuestion
      { _tqKey      :: QuestionKey
      , _tqQuestion :: String
      , _tqValue    :: Maybe String
      , _tqError    :: Maybe String
      }

  deriving (Eq, Show)

printQuestions :: [Question] -> IO ()
printQuestions
  = mapM_ (putStrLn . renderQuestion)

renderQuestion :: Question -> String
renderQuestion
  = unlines . \case
      GBPQ GBPQuestion{..} ->
        [ _questionKeyString _gbpqKey
        , "  " ++ _gbpqQuestion
        , "  > " ++ maybe "" id _gbpqValue
        , maybe "" ("    ^ " ++) _gbpqError
        ]
      TextQ TextQuestion{..} ->
        [ _questionKeyString _tqKey
        , "  " ++ _tqQuestion
        , "  > " ++ maybe "" id _tqValue
        , maybe "" ("    ^ " ++) _tqError
        ]
