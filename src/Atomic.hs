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

  , BagA
  , runBagA
  , lookupValid

  , BagM
  , runBagM
  , independently
  , lookupValidMaybe
  , lookupValidResult
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

type BagF fields a
  = BagState fields -> (Maybe a, BagState fields)

data BagState fields
  = BagState
      { _bsRead  :: Bag Unvalidated fields
      , _bsWrite :: Bag ValidLookupResult fields
      }

runBagF
  :: Bag Unvalidated fields
  -> BagF fields a
  -> (Maybe a, Bag ValidLookupResult fields)

runBagF b k
  = let s         = BagState b empty
        (mx, s')  = k s

    in  (mx, _bsWrite s')

fmapBagF :: (a -> b) -> BagF fields a -> BagF fields b
fmapBagF f k
  = \s -> let (mx, s') = k s in (fmap f mx, s')

pureBagF :: a -> s -> (Maybe a, s)
pureBagF
  = (,) . Just

writeResult
  :: forall name fields ty.
     HasField fields name ty
  => ValidLookupResult ty
  -> BagState fields
  -> BagState fields

writeResult r s
  = s { _bsWrite = insert @name r (_bsWrite s) }

newtype BagA fields a
  = BagA { _runBagA :: BagF fields a }

runBagA
  :: Bag Unvalidated fields
  -> BagA fields a
  -> (Maybe a, Bag ValidLookupResult fields)

runBagA b (BagA k)
  = runBagF b k

instance Functor (BagA fields) where
  fmap f (BagA k)
    = BagA (fmapBagF f k)

instance Applicative (BagA fields) where
  pure
    = BagA . pureBagF

  BagA kf <*> BagA kx
    = BagA $ \s ->
        let (mf, s')  = kf s
            (mx, s'') = kx s'

        in  (mf <*> mx, s'')

lookupValid
  :: forall name fields ty.
     (HasField fields name ty,
      Valid ty)

  => BagA fields ty

lookupValid
  = BagA $ \s ->
      case lookup @name (_bsRead s) of
        Nothing ->
          (Nothing, writeResult @name MissingField s)
        Just (Unvalidated x) ->
          case validatePlain x of
            Failure e ->
              (Nothing, writeResult @name (InvalidField (x, e)) s)
            Success y ->
              (Just y, writeResult @name (ValidField y) s)

newtype BagM fields a
  = BagM { _runBagM :: BagF fields a }

runBagM
  :: Bag Unvalidated fields
  -> BagM fields a
  -> (Maybe a, Bag ValidLookupResult fields)

runBagM b (BagM k)
  = runBagF b k

instance Functor (BagM fields) where
  fmap f (BagM k)
    = BagM (fmapBagF f k)

instance Applicative (BagM fields) where
  pure
    = return
  (<*>)
    = ap

instance Monad (BagM fields) where
  return
    = BagM . pureBagF
  BagM k >>= f
    = BagM $ \s ->
        let (mx, s') = k s
        in  case mx of
              Nothing ->
                (Nothing, s')
              Just x ->
                _runBagM (f x) s'

independently :: BagA fields a -> BagM fields a
independently (BagA k)
  = BagM k

lookupValidMaybe
  :: forall name fields ty.
     (HasField fields name ty,
      Valid ty)

  => BagM fields (Maybe ty)

lookupValidMaybe
  = validLookupResultToMaybe <$> lookupValidResult @name

lookupValidResult
  :: forall name fields ty.
     (HasField fields name ty,
      Valid ty)

  => BagM fields (ValidLookupResult ty)

lookupValidResult
  = BagM $ \s ->
      case lookup @name (_bsRead s) of
        Nothing ->
          writeAndReturnResult @name MissingField s
        Just (Unvalidated x) ->
          case validatePlain x of
            Failure e ->
              writeAndReturnResult @name (InvalidField (x, e)) s
            Success y ->
              writeAndReturnResult @name (ValidField y) s

writeAndReturnResult
  :: forall name fields ty.
     HasField fields name ty
  => ValidLookupResult ty
  -> BagState fields
  -> (Maybe (ValidLookupResult ty), BagState fields)

writeAndReturnResult r s
  = (Just r, writeResult @name r s)
