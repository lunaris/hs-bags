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
  , validateContextual
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
  = MissingKey
  | InvalidKey (Plain a, PlainError a)
  | ValidKey a

validLookupResultToMaybe :: ValidLookupResult a -> Maybe a
validLookupResultToMaybe
  = \case
      MissingKey    -> Nothing
      InvalidKey _  -> Nothing
      ValidKey x    -> Just x

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
        MissingKey ->
          object
            [ "type"  .= ("MissingKey" :: String)
            , "value" .= ()
            ]

        InvalidKey (x, e) ->
          object
            [ "type"  .= ("InvalidKey" :: String)
            , "value" .= object
                [ "plain" .= x
                , "error" .= e
                ]

            ]

        ValidKey y ->
          object
            [ "type"  .= ("ValidKey" :: String)
            , "value" .= y
            ]

insertPlain
  :: forall name keys ty.
     (HasKey keys name ty,
      Valid ty)

  => Plain ty
  -> Bag Unvalidated keys
  -> Bag Unvalidated keys

insertPlain
  = insert @name . Unvalidated

newtype Builder keys contextuals a
  = Builder
      { _runBuilder
          :: BagState keys contextuals
          -> (Maybe a, BagState keys contextuals)

      }

data BagState keys contextuals
  = BagState
      { _bsUnvalidated :: Bag Unvalidated keys
      , _bsLookups     :: Bag ValidLookupResult keys
      , _bsContextuals :: Bag Identity contextuals
      }

runBuilder
  :: forall keys contextuals a.
     Bag Unvalidated keys
  -> Builder keys contextuals a
  -> (Maybe a, Bag ValidLookupResult keys, Bag Identity contextuals)

runBuilder b (Builder k)
  = let s         = BagState b empty empty
        (mx, s')  = k s

    in  (mx, _bsLookups s', _bsContextuals s')

writeLookupResult
  :: forall name keys contextuals ty.
     HasKey keys name ty
  => ValidLookupResult ty
  -> BagState keys contextuals
  -> BagState keys contextuals

writeLookupResult r s
  = s { _bsLookups = insert @name r (_bsLookups s) }

writeAndReturnLookupResult
  :: forall name keys contextuals ty.
     HasKey keys name ty
  => ValidLookupResult ty
  -> BagState keys contextuals
  -> (Maybe (ValidLookupResult ty), BagState keys contextuals)

writeAndReturnLookupResult r s
  = (Just r, writeLookupResult @name r s)

writeContextualResult
  :: forall name keys contextuals err.
     HasKey contextuals name err
  => err
  -> BagState keys contextuals
  -> BagState keys contextuals

writeContextualResult e s
  = s { _bsContextuals = insertValue @name e (_bsContextuals s) }

instance Functor (Builder keys contextuals) where
  fmap f (Builder k)
    = Builder (\s -> let (mx, s') = k s in (fmap f mx, s'))

instance Applicative (Builder keys contextuals) where
  pure
    = Builder . (,) . Just

  Builder kf <*> Builder kx
    = Builder $ \s ->
        let (mf, s')  = kf s
            (mx, s'') = kx s'

        in  (mf <*> mx, s'')

andThen
  :: Builder keys contextuals a
  -> (a -> Builder keys contextuals b)
  -> Builder keys contextuals b

andThen (Builder k) h
  = Builder $ \s ->
      case k s of
        (Nothing, s') ->
          (Nothing, s')
        (Just x, s') ->
          _runBuilder (h x) s'

validateContextual
  :: forall name keys contextuals e ty.
     HasKey contextuals name e
  => Validation e ty
  -> Builder keys contextuals ty

validateContextual v
  = Builder $ \s ->
      case v of
        Failure e ->
          (Nothing, writeContextualResult @name e s)
        Success x ->
          (Just x, s)

requireValid
  :: forall name keys contextuals ty.
     (HasKey keys name ty,
      Valid ty)

  => Builder keys contextuals ty

requireValid
  = Builder $ \s ->
      case lookup @name (_bsUnvalidated s) of
        Nothing ->
          (Nothing, writeLookupResult @name MissingKey s)
        Just (Unvalidated x) ->
          case validatePlain x of
            Failure e ->
              (Nothing, writeLookupResult @name (InvalidKey (x, e)) s)
            Success y ->
              (Just y, writeLookupResult @name (ValidKey y) s)

lookupValid
  :: forall name keys contextuals ty.
     (HasKey keys name ty,
      Valid ty)

  => Builder keys contextuals (ValidLookupResult ty)

lookupValid
  = Builder $ \s ->
      case lookup @name (_bsUnvalidated s) of
        Nothing ->
          writeAndReturnLookupResult @name MissingKey s
        Just (Unvalidated x) ->
          case validatePlain x of
            Failure e ->
              writeAndReturnLookupResult @name (InvalidKey (x, e)) s
            Success y ->
              writeAndReturnLookupResult @name (ValidKey y) s
