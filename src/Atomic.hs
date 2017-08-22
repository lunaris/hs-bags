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
  ( Atomic (..)
  , Unvalidated (..)

  , insertPlain
  , lookupAtom
  ) where

import Operations
import Types
import Validation

import Data.Aeson hiding (Result(..))
import Prelude    hiding (lookup)

class Atomic a where
  type Plain a           :: *
  type ValidationError a :: *
  validateAtom           :: Plain a -> Validation (ValidationError a) a
  unvalidateAtom         :: a -> Plain a

newtype Unvalidated a
  = Unvalidated { getUnvalidated :: Plain a }

deriving instance Eq (Plain a) => Eq (Unvalidated a)
deriving instance Ord (Plain a) => Ord (Unvalidated a)
deriving instance Show (Plain a) => Show (Unvalidated a)
deriving instance ToJSON (Plain a) => ToJSON (Unvalidated a)

insertPlain
  :: forall name fields ty.
     (HasField fields name ty,
      Atomic ty)

  => Plain ty
  -> Bag Unvalidated fields
  -> Bag Unvalidated fields

insertPlain
  = insert @name . Unvalidated

lookupAtom
  :: forall name fields ty.
     (HasField fields name ty,
      Atomic ty)

  => Bag Unvalidated fields
  -> Maybe ty

lookupAtom b
  = lookup @name b >>= \(Unvalidated x) ->
      case validateAtom x of
        Failure _ -> Nothing
        Success y -> Just y
