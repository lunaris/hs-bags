{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Atomic
  ( Atomic (..)
  , UnvalidatedFields

  --, lookupAtom
  ) where

--import Operations
import Types
import Validation

class Atomic a where
  type Unvalidated a     :: *
  type ValidationError a :: *
  validateAtom           :: Unvalidated a -> Validation (ValidationError a) a
  unvalidateAtom         :: a -> Unvalidated a

type family UnvalidatedFields (fields :: [*]) :: [*] where
  UnvalidatedFields (Field name subfields ': fields)
    = Field name (UnvalidatedFields subfields) ': UnvalidatedFields fields
  UnvalidatedFields (Field name ty ': fields)
    = Field name (Unvalidated ty) ': UnvalidatedFields fields
  UnvalidatedFields '[]
    = '[]

{-
lookupAtom
  :: forall name fields ty.
     (HasField fields name ty,
      Atomic ty)
  => Bag fields
  -> Maybe
  -}
