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
  , validateComposite
  , requireValid
  , lookupValid
  ) where

import Operations
import Types
import Validation

import Control.Monad
import Data.Aeson            hiding (Result(..))
import Data.Functor.Identity
import Prelude               hiding (lookup)

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

newtype Builder fields composites a
  = Builder
      { _runBuilder
          :: BagState fields composites
          -> (Maybe a, BagState fields composites)

      }

data BagState fields composites
  = BagState
      { _bsUnvalidated :: Bag Unvalidated fields
      , _bsLookups     :: Bag ValidLookupResult fields
      , _bsComposites  :: Bag Identity composites
      }

runBuilder
  :: forall fields composites a.
     Bag Unvalidated fields
  -> Builder fields composites a
  -> (Maybe a, Bag ValidLookupResult fields, Bag Identity composites)

runBuilder b (Builder k)
  = let s         = BagState b empty empty
        (mx, s')  = k s

    in  (mx, _bsLookups s', _bsComposites s')

writeLookupResult
  :: forall name fields composites ty.
     HasField fields name ty
  => ValidLookupResult ty
  -> BagState fields composites
  -> BagState fields composites

writeLookupResult r s
  = s { _bsLookups = insert @name r (_bsLookups s) }

writeAndReturnLookupResult
  :: forall name fields composites ty.
     HasField fields name ty
  => ValidLookupResult ty
  -> BagState fields composites
  -> (Maybe (ValidLookupResult ty), BagState fields composites)

writeAndReturnLookupResult r s
  = (Just r, writeLookupResult @name r s)

writeCompositeResult
  :: forall name fields composites err.
     HasField composites name err
  => err
  -> BagState fields composites
  -> BagState fields composites

writeCompositeResult e s
  = s { _bsComposites = insertValue @name e (_bsComposites s) }

instance Functor (Builder fields composites) where
  fmap f (Builder k)
    = Builder (\s -> let (mx, s') = k s in (fmap f mx, s'))

instance Applicative (Builder fields composites) where
  pure
    = Builder . (,) . Just

  Builder kf <*> Builder kx
    = Builder $ \s ->
        let (mf, s')  = kf s
            (mx, s'') = kx s'

        in  (mf <*> mx, s'')

andThen
  :: Builder fields composites a
  -> (a -> Builder fields composites b)
  -> Builder fields composites b

andThen (Builder k) h
  = Builder $ \s ->
      case k s of
        (Nothing, s') ->
          (Nothing, s')
        (Just x, s') ->
          _runBuilder (h x) s'

validateComposite
  :: forall name fields composites e ty.
     HasField composites name e
  => Validation e ty
  -> Builder fields composites ty

validateComposite v
  = Builder $ \s ->
      case v of
        Failure e ->
          (Nothing, writeCompositeResult @name e s)
        Success x ->
          (Just x, s)

requireValid
  :: forall name fields composites ty.
     (HasField fields name ty,
      Valid ty)

  => Builder fields composites ty

requireValid
  = Builder $ \s ->
      case lookup @name (_bsUnvalidated s) of
        Nothing ->
          (Nothing, writeLookupResult @name MissingField s)
        Just (Unvalidated x) ->
          case validatePlain x of
            Failure e ->
              (Nothing, writeLookupResult @name (InvalidField (x, e)) s)
            Success y ->
              (Just y, writeLookupResult @name (ValidField y) s)

lookupValid
  :: forall name fields composites ty.
     (HasField fields name ty,
      Valid ty)

  => Builder fields composites (ValidLookupResult ty)

lookupValid
  = Builder $ \s ->
      case lookup @name (_bsUnvalidated s) of
        Nothing ->
          writeAndReturnLookupResult @name MissingField s
        Just (Unvalidated x) ->
          case validatePlain x of
            Failure e ->
              writeAndReturnLookupResult @name (InvalidField (x, e)) s
            Success y ->
              writeAndReturnLookupResult @name (ValidField y) s
