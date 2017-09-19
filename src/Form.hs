{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE UndecidableInstances   #-}

module Form
  ( Form (..)
  , FormField (..)

  , renderForm
  , renderGroup
  , renderFields

  , QuestionKey (..)

  , Question (..)
  , GBPQuestion (..)
  , TextQuestion (..)

  , Answer (..)
  , AnswerValue (..)
  , FromAnswerValue (..)
  , fromAnswers

  , printQuestions
  ) where

import All
import Assocs
import Atomic
import HList
import Operations
import Types

import           Data.Coerce
import           Data.Functor.Identity
import           Data.Kind
import           Data.Maybe
import           Data.Proxy
import qualified Data.Text             as Tx
import           Data.Typeable
import           GHC.TypeLits
import           Prelude               hiding (lookup)

class All (FormGroup form) (FormGroups form)
  =>  Form form where

  type FormFields form      :: [Assoc Type]
  type FormContextuals form :: [Assoc Type]
  type FormGroups form      :: [Assoc [Key]]

class All (FormField form) (Snd ga)
  =>  FormGroup form (ga :: (Key, [Key]))

instance All (FormField form) (Snd ga)
      => FormGroup form ga

class (Form form,
       HasKey' (FormFields form) fk,
       HasKeys' (FormContextuals form) (ContextualKeys form fk))

    => FormField form fk where

  type ContextualKeys form fk :: [Key]

  toFormField
    :: form
    -> ValidLookupResult (KeyValue (FormFields form) fk)
    -> UncurryF Maybe
        (KeysValues (FormContextuals form) (ContextualKeys form fk)) Question

renderForm
  :: forall form.
     Form form
  => form
  -> Bag ValidLookupResult (FormFields form)
  -> Bag Identity (FormContextuals form)
  -> [Question]

renderForm form fs cs
  = withAll @_ @(FormGroup form) @(FormGroups form) k
  where
    k :: forall x. FormGroup form x => Proxy x -> [Question]
    k _
      = renderFields @(Snd x) form fs cs

renderGroup
  :: forall gk form gfks.
     (Form form,
      HasKey (FormGroups form) gk gfks,
      All (FormField form) gfks)

  => form
  -> Bag ValidLookupResult (FormFields form)
  -> Bag Identity (FormContextuals form)
  -> [Question]

renderGroup
  = renderFields @gfks

renderFields
  :: forall fks form.
     (Form form,
      All (FormField form) fks)

  => form
  -> Bag ValidLookupResult (FormFields form)
  -> Bag Identity (FormContextuals form)
  -> [Question]

renderFields form fs cs
  = withAll @_ @(FormField form) @fks h
  where
    h :: forall fk. FormField form fk => Proxy fk -> [Question]
    h _
      = case lookup @fk fs of
          Nothing ->
            []
          Just r ->
            [ curryHListF (toFormField @_ @fk form r)
                (lookupAll @(ContextualKeys form fk) cs)

            ]

newtype QuestionKey
  = QuestionKey { _questionKeyText :: Tx.Text }
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

data Answer
  = Answer
      { _aKey   :: QuestionKey
      , _aValue :: AnswerValue
      }

  deriving (Eq, Show)

data AnswerValue
  = TextualAV String
  deriving (Eq, Show)

class (KnownSymbol (Fst a),
       Typeable f,
       Typeable (f (Snd a)),
       FromAnswerValue (f (Snd a)),
       HasKey as (Fst a) (Snd a))

    => AssocFromAnswer (f :: Type -> Type) (as :: [Assoc Type]) (a :: Assoc Type) where

  assocFromAnswer :: QuestionKey -> AnswerValue -> Proxy a -> Bag f as

instance (KnownSymbol (Fst a),
          Typeable f,
          Typeable (f (Snd a)),
          FromAnswerValue (f (Snd a)),
          HasKey as (Fst a) (Snd a))

      =>  AssocFromAnswer (f :: Type -> Type) (as :: [Assoc Type]) (a :: Assoc Type) where

  assocFromAnswer (QuestionKey qk) av _
    | keyText @(Fst a) == qk
        = maybe mempty (singleton @(Fst a)) (fromAnswerValue av)
    | otherwise
        = mempty

class FromAnswerValue a where
  fromAnswerValue :: AnswerValue -> Maybe a

instance FromAnswerValue String where
  fromAnswerValue
    = \case
        TextualAV s ->
          Just s

instance FromAnswerValue Tx.Text where
  fromAnswerValue
    = \case
        TextualAV s ->
          Just (Tx.pack s)

instance FromAnswerValue Int where
  fromAnswerValue
    = \case
        TextualAV s ->
          case reads s of
            [(x, "")] -> Just x
            _         -> Nothing

instance FromAnswerValue a => FromAnswerValue (Identity a) where
  fromAnswerValue
    = coerce @(AnswerValue -> Maybe a) fromAnswerValue

instance (Valid a, FromAnswerValue (Plain a))
      =>  FromAnswerValue (Unvalidated a) where

  fromAnswerValue
    = coerce @(AnswerValue -> Maybe (Plain a)) fromAnswerValue

fromAnswers
  :: forall f as.
     All (AssocFromAnswer f as) as
  => [Answer]
  -> Bag f as

fromAnswers
  = foldMap h
  where
    h (Answer k v)
      = withAll @_ @(AssocFromAnswer f as) @as (assocFromAnswer k v)

printQuestions :: [Question] -> IO ()
printQuestions
  = mapM_ (putStrLn . renderQuestion)

renderQuestion :: Question -> String
renderQuestion
  = unlines . \case
      GBPQ GBPQuestion{..} ->
        [ Tx.unpack (_questionKeyText _gbpqKey)
        , "  " ++ _gbpqQuestion
        , "  > " ++ maybe "" id _gbpqValue
        , maybe "" ("    ^ " ++) _gbpqError
        ]
      TextQ TextQuestion{..} ->
        [ Tx.unpack (_questionKeyText _tqKey)
        , "  " ++ _tqQuestion
        , "  > " ++ maybe "" id _tqValue
        , maybe "" ("    ^ " ++) _tqError
        ]
