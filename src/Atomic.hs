{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
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

  , insertPlain
  , lookupValid
  , ValidLookupResult (..)
  ) where

import Operations
import Types
import Validation

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.Aeson           hiding (Result(..))
import Prelude              hiding (lookup)

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

insertPlain
  :: forall name fields ty.
     (HasField fields name ty,
      Valid ty)

  => Plain ty
  -> Bag Unvalidated fields
  -> Bag Unvalidated fields

insertPlain
  = insert @name . Unvalidated

lookupValid
  :: forall name fields ty.
     (HasField fields name ty,
      Valid ty)

  => Bag Unvalidated fields
  -> MaybeT (State (Bag ValidLookupResult fields)) ty

lookupValid bag
  = case lookup @name bag of
      Nothing -> do
        lift $ modify (insert @name MissingField)
        MaybeT $ pure Nothing

      Just (Unvalidated x) ->
        case validatePlain x of
          Failure e -> do
            lift $ modify (insert @name (InvalidField (x, e)))
            MaybeT $ pure Nothing

          Success y -> do
            lift $ modify (insert @name (ValidField y))
            MaybeT $ pure (Just y)

{-
MonadReader (Bag Unvalidated fields) m
MonadState (Bag ValidLookupResult fields) m
MonadMaybe
-}

data ValidLookupResult a
  = MissingField
  | InvalidField (Plain a, PlainError a)
  | ValidField a

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
