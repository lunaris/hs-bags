{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

import Control.Monad.Reader
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
  :: forall name fields ty m.
     (MonadReader (Bag Unvalidated fields) m,
      HasField fields name ty,
      Valid ty)

  => m (ValidLookupResult ty)

lookupValid
  = maybe MissingField k <$> lookup @name
  where
    k (Unvalidated x)
      = case validatePlain x of
          Failure e -> InvalidField (x, e)
          Success y -> ValidField y

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
