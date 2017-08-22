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
  ) where

import Operations
import Types
import Validation

import Data.Aeson hiding (Result(..))
import Prelude    hiding (lookup)

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
  -> ValidLookupError ty

lookupValid
  = maybe MissingField
      (validating InvalidField ValidField . validatePlain . getUnvalidated)

  . lookup @name

data ValidLookupError a
  = MissingField
  | InvalidField (PlainError a)
  | ValidField a

deriving instance (Eq (PlainError a), Eq a) => Eq (ValidLookupError a)
deriving instance (Ord (PlainError a), Ord a) => Ord (ValidLookupError a)
deriving instance (Show (PlainError a), Show a) => Show (ValidLookupError a)
