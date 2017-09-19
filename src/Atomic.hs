{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
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
  , lookupValid

  , Builder
  , runBuilder
  , andThen
  , validateContextual
  , requireValid
  , checkValid
  ) where

import Assocs
import Operations
import Types
import Validation

import Control.Monad
import Control.Monad.Reader
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
  :: forall k as ty.
     (HasKey as k ty,
      Valid ty)

  => Plain ty
  -> Bag Unvalidated as
  -> Bag Unvalidated as

insertPlain
  = insert @k . Unvalidated

lookupValid
  :: forall k as ty m.
     (MonadReader (Bag Unvalidated as) m,
      HasKey as k ty,
      Valid ty)

  => m (ValidLookupResult ty)

lookupValid
  = fmap k (lookup @k)
  where
    k
      = \case
          Nothing ->
            MissingKey
          Just (Unvalidated x) ->
            case validatePlain x of
              Failure e ->
                InvalidKey (x, e)
              Success y ->
                ValidKey y

newtype Builder fas cas a
  = Builder { _runBuilder :: BagState fas cas -> (Maybe a, BagState fas cas) }

data BagState fas cas
  = BagState
      { _bsUnvalidated :: Bag Unvalidated fas
      , _bsLookups     :: Bag ValidLookupResult fas
      , _bsContextuals :: Bag Identity cas
      }

runBuilder
  :: forall fas cas a.
     Bag Unvalidated fas
  -> Builder fas cas a
  -> (Maybe a, Bag ValidLookupResult fas, Bag Identity cas)

runBuilder b (Builder k)
  = let s         = BagState b mempty mempty
        (mx, s')  = k s

    in  (mx, _bsLookups s', _bsContextuals s')

writeLookupResult
  :: forall k fas cas ty.
     HasKey fas k ty
  => ValidLookupResult ty
  -> BagState fas cas
  -> BagState fas cas

writeLookupResult r s
  = s { _bsLookups = insert @k r (_bsLookups s) }

writeAndReturnLookupResult
  :: forall k fas cas ty.
     HasKey fas k ty
  => ValidLookupResult ty
  -> BagState fas cas
  -> (Maybe (ValidLookupResult ty), BagState fas cas)

writeAndReturnLookupResult r s
  = (Just r, writeLookupResult @k r s)

writeContextualResult
  :: forall k fas cas err.
     HasKey cas k err
  => err
  -> BagState fas cas
  -> BagState fas cas

writeContextualResult e s
  = s { _bsContextuals = insertValue @k e (_bsContextuals s) }

instance Functor (Builder fas cas) where
  fmap f (Builder k)
    = Builder (\s -> let (mx, s') = k s in (fmap f mx, s'))

instance Applicative (Builder fas cas) where
  pure
    = Builder . (,) . Just

  Builder kf <*> Builder kx
    = Builder $ \s ->
        let (mf, s')  = kf s
            (mx, s'') = kx s'

        in  (mf <*> mx, s'')

andThen
  :: Builder fas cas a
  -> (a -> Builder fas cas b)
  -> Builder fas cas b

andThen (Builder k) h
  = Builder $ \s ->
      case k s of
        (Nothing, s') ->
          (Nothing, s')
        (Just x, s') ->
          _runBuilder (h x) s'

validateContextual
  :: forall k fas cas e ty.
     HasKey cas k e
  => Validation e ty
  -> Builder fas cas ty

validateContextual v
  = Builder $ \s ->
      case v of
        Failure e ->
          (Nothing, writeContextualResult @k e s)
        Success x ->
          (Just x, s)

requireValid
  :: forall k fas cas ty.
     (HasKey fas k ty,
      Valid ty)

  => Builder fas cas ty

requireValid
  = Builder $ \s ->
      case lookup @k (_bsUnvalidated s) of
        Nothing ->
          (Nothing, writeLookupResult @k MissingKey s)
        Just (Unvalidated x) ->
          case validatePlain x of
            Failure e ->
              (Nothing, writeLookupResult @k (InvalidKey (x, e)) s)
            Success y ->
              (Just y, writeLookupResult @k (ValidKey y) s)

checkValid
  :: forall k fas cas ty.
     (HasKey fas k ty,
      Valid ty)

  => Builder fas cas (ValidLookupResult ty)

checkValid
  = Builder $ \s ->
      case lookup @k (_bsUnvalidated s) of
        Nothing ->
          writeAndReturnLookupResult @k MissingKey s
        Just (Unvalidated x) ->
          case validatePlain x of
            Failure e ->
              writeAndReturnLookupResult @k (InvalidKey (x, e)) s
            Success y ->
              writeAndReturnLookupResult @k (ValidKey y) s
