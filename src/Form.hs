{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TypeFamilies           #-}

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

import Atomic
import HList
import Types

class Form form where
  type FormFields form      :: [*]
  type FormContextuals form :: [*]

class (Form form,
       HasKey' (FormFields form) fieldName,
       HasKeys' (FormContextuals form) contextualNames)

    => FormField form fieldName contextualNames where

  toFormField
    :: form
    -> ValidLookupResult (KeyType (FormFields form) fieldName)
    -> UncurryF Maybe (KeysTypes (FormContextuals form) contextualNames) Question

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
