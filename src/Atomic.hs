{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Atomic
  ( Valid (..)
  , Unvalidated (..)

  , ValidLookupResult (..)
  , validLookupResultToMaybe

  , insertPlain

  , Builder
  , runBuilder
  , andThen
  , requireValid
  , lookupValid
  ) where

import Operations
import Types
import Validation

import Control.Monad
import Data.Aeson    hiding (Result(..))
import Prelude       hiding (lookup)

class Valid a where
  type Plain a      :: *
  type PlainError a :: *
  validatePlain     :: Plain a -> Validation (PlainError a) a
  unvalidateValid   :: a -> Plain a

newtype Unvalidated a
  = Unvalidated { getUnvalidated :: Plain a }

deriving instance Eq (Plain a) => Eq (Unvalidated a)
deriving instance Ord (Plain a) => Ord (Unvalidated a)
deriving instance Show (Plain a) => Show (Unvalidated a)
deriving instance ToJSON (Plain a) => ToJSON (Unvalidated a)

data ValidLookupResult a
  = MissingField
  | InvalidField (Plain a, PlainError a)
  | ValidField a

validLookupResultToMaybe :: ValidLookupResult a -> Maybe a
validLookupResultToMaybe
  = \case
      MissingField    -> Nothing
      InvalidField _  -> Nothing
      ValidField x    -> Just x

deriving instance (Eq (Plain a), Eq (PlainError a), Eq a)
                => Eq (ValidLookupResult a)

deriving instance (Ord (Plain a), Ord (PlainError a), Ord a)
                => Ord (ValidLookupResult a)

deriving instance (Show (Plain a), Show (PlainError a), Show a)
                => Show (ValidLookupResult a)

instance (ToJSON (Plain a), ToJSON (PlainError a), ToJSON a)
      =>  ToJSON (ValidLookupResult a) where

  toJSON
    = \case
        MissingField ->
          object
            [ "type"  .= ("MissingField" :: String)
            , "value" .= ()
            ]

        InvalidField (x, e) ->
          object
            [ "type"  .= ("InvalidField" :: String)
            , "value" .= object
                [ "plain" .= x
                , "error" .= e
                ]

            ]

        ValidField y ->
          object
            [ "type"  .= ("ValidField" :: String)
            , "value" .= y
            ]

insertPlain
  :: forall name fields ty.
     (HasField fields name ty,
      Valid ty)

  => Plain ty
  -> Bag Unvalidated fields
  -> Bag Unvalidated fields

insertPlain
  = insert @name . Unvalidated

newtype Builder fields a
  = Builder { _runBuilder :: BagState fields -> (Maybe a, BagState fields) }

data BagState fields
  = BagState
      { _bsRead  :: Bag Unvalidated fields
      , _bsWrite :: Bag ValidLookupResult fields
      }

runBuilder
  :: Bag Unvalidated fields
  -> Builder fields a
  -> (Maybe a, Bag ValidLookupResult fields)

runBuilder b (Builder k)
  = let s         = BagState b empty
        (mx, s')  = k s

    in  (mx, _bsWrite s')

writeResult
  :: forall name fields ty.
     HasField fields name ty
  => ValidLookupResult ty
  -> BagState fields
  -> BagState fields

writeResult r s
  = s { _bsWrite = insert @name r (_bsWrite s) }

writeAndReturnResult
  :: forall name fields ty.
     HasField fields name ty
  => ValidLookupResult ty
  -> BagState fields
  -> (Maybe (ValidLookupResult ty), BagState fields)

writeAndReturnResult r s
  = (Just r, writeResult @name r s)

instance Functor (Builder fields) where
  fmap f (Builder k)
    = Builder (\s -> let (mx, s') = k s in (fmap f mx, s'))

instance Applicative (Builder fields) where
  pure
    = Builder . (,) . Just

  Builder kf <*> Builder kx
    = Builder $ \s ->
        let (mf, s')  = kf s
            (mx, s'') = kx s'

        in  (mf <*> mx, s'')

andThen :: Builder fields a -> (a -> Builder fields b) -> Builder fields b
andThen (Builder k) h
  = Builder $ \s ->
      case k s of
        (Nothing, s') ->
          (Nothing, s')
        (Just x, s') ->
          _runBuilder (h x) s'

requireValid
  :: forall name fields ty.
     (HasField fields name ty,
      Valid ty)

  => Builder fields ty

requireValid
  = Builder $ \s ->
      case lookup @name (_bsRead s) of
        Nothing ->
          (Nothing, writeResult @name MissingField s)
        Just (Unvalidated x) ->
          case validatePlain x of
            Failure e ->
              (Nothing, writeResult @name (InvalidField (x, e)) s)
            Success y ->
              (Just y, writeResult @name (ValidField y) s)

lookupValid
  :: forall name fields ty.
     (HasField fields name ty,
      Valid ty)

  => Builder fields (ValidLookupResult ty)

lookupValid
  = Builder $ \s ->
      case lookup @name (_bsRead s) of
        Nothing ->
          writeAndReturnResult @name MissingField s
        Just (Unvalidated x) ->
          case validatePlain x of
            Failure e ->
              writeAndReturnResult @name (InvalidField (x, e)) s
            Success y ->
              writeAndReturnResult @name (ValidField y) s
