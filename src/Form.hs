{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}

module Form
  ( Form (..)
  , FormField (..)

  , render

  , QuestionKey (..)

  , Question (..)
  , GBPQuestion (..)
  , TextQuestion (..)

  , printQuestions
  , renderQuestion
  ) where

import All
import Assocs
import Atomic
import HList
import Operations
import Types

import Data.Functor.Identity
import Data.Kind
import Data.Maybe
import Data.Proxy
import Prelude               hiding (lookup)

class Form form where
  type FormFields form      :: [Assoc Type]
  type FormContextuals form :: [Assoc Type]
  type FormGroups form      :: [Assoc [Key]]

class (Form form,
       HasKey (FormFields form) fk (KeyValue (FormFields form) fk),
       HasKeys (FormContextuals form) (ContextualKeys form fk)
        (KeysValues (FormContextuals form) (ContextualKeys form fk)))

    => FormField form fk where

  type ContextualKeys form fk :: [Key]

  toFormField
    :: form
    -> ValidLookupResult (KeyValue (FormFields form) fk)
    -> UncurryF Maybe
        (KeysValues (FormContextuals form) (ContextualKeys form fk)) [Question]

render
  :: forall fks form.
     (Form form,
      All (FormField form) fks)

  => form
  -> Bag ValidLookupResult (FormFields form)
  -> Bag Identity (FormContextuals form)
  -> [Question]

render form fs cs
  = withAll @_ @(FormField form) @fks h
  where
    h :: forall fk. FormField form fk => Proxy (fk :: Key) -> [Question]
    h _
      = curryHListF (toFormField @_ @fk form r) (lookupAll @(ContextualKeys form fk) cs)
      where
        r = fromMaybe MissingKey (lookup @fk fs)

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
