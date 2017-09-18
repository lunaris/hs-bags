{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Assocs
  ( Key
  , keyText

  , Assoc
  , Fst
  , Snd

  , HasKeys' (..)
  , HasKeys

  , HasKey' (..)
  , HasKey
  ) where

import All
import HList

import           Data.Kind
import qualified Data.Text     as Tx
import           Data.Typeable
import           GHC.TypeLits

type Key
  = Symbol

keyText :: forall k. KnownSymbol k => Tx.Text
keyText
  = Tx.pack (symbolVal (Proxy @k))

type Assoc value
  = (Key, value)

type family Fst (t :: (a, b)) :: a where
  Fst '(a, b)
    = a

type family Snd (t :: (a, b)) :: b where
  Snd '(a, b)
    = b

class (All (HasKey' as) ks,
       Arguments (KeysValues as ks))
    => HasKeys' (as :: [Assoc value]) (ks :: [Key]) where

  type KeysValues as ks :: [value]

instance (LookupAll as ks ~ maybeVs,
          maybeVs ~ 'Just vs,
          All (HasKey' as) ks,
          All Typeable vs,
          Arguments vs,
          ErrorWhenNoValues as ks maybeVs)

      =>  HasKeys' (as :: [Assoc value]) (ks :: [Key]) where

  type KeysValues as ks
    = FromJust (LookupAll as ks)

class (HasKeys' as ks,
       KeysValues as ks ~ vs)

    => HasKeys (as :: [Assoc value]) (ks :: [Key]) (vs :: [value]) | as ks -> vs

instance (HasKeys' as ks,
          KeysValues as ks ~ vs)

      =>  HasKeys (as :: [Assoc value]) (ks :: [Key]) (vs :: [value])

type family LookupAll (as:: [Assoc value]) (ks :: [Key]) :: Maybe [value] where
  LookupAll as (k ': ks)
    = LiftConsMaybe (Lookup as k) (LookupAll as ks)
  LookupAll as '[]
    = 'Just '[]

type family LiftConsMaybe (a :: Maybe k) (as :: Maybe [k]) :: Maybe [k] where
  LiftConsMaybe ('Just a) ('Just as)
    = 'Just (a ': as)
  LiftConsMaybe _ _
    = 'Nothing

type family ErrorWhenNoValues
     (as :: [Assoc value])
     (ks :: [Key])
     (maybeVs :: Maybe [value])
  :: Constraint where

  ErrorWhenNoValues _ _ ('Just _)
    = ()
  ErrorWhenNoValues as ks 'Nothing
    = TypeError
        (     'Text "The keys: "
        ':$$: KeysError ('Text "  ") ks
        ':$$: 'Text " could not be found. The available keys are:"
        ':$$: AssocKeysError ('Text "  ") as
        )

type family KeysError (prefix :: ErrorMessage) (ks :: [Key]) :: ErrorMessage where
  KeysError prefix '[k]
    =     prefix
    ':<>: 'Text k
  KeysError prefix (k ': ks)
    =     prefix
    ':<>: 'Text k
    ':$$: KeysError prefix ks

class (KnownSymbol k, Typeable (KeyValue as k))
    => HasKey' (as :: [Assoc value]) (k :: Key) where

  type KeyValue as k :: value

instance (KnownSymbol k,
          Lookup as k ~ maybeV,
          maybeV ~ 'Just v,
          Typeable v,
          ErrorWhenNoValue as k maybeV)

      =>  HasKey' (as :: [Assoc value]) (k :: Key) where

  type KeyValue as k
    = FromJust (Lookup as k)

class (HasKey' as k,
       KeyValue as k ~ v)

    => HasKey (as :: [Assoc value]) (k :: Key) (v :: value) | as k -> v

instance (HasKey' as k,
          KeyValue as k ~ v)

      =>  HasKey (as :: [Assoc value]) (k :: Key) (v :: value)

type family Lookup (as :: [Assoc value]) (k :: Key) :: Maybe value where
  Lookup ('(k, v) ': as) k
    = 'Just v
  Lookup (_ ': as) k
    = Lookup as k
  Lookup '[] _
    = 'Nothing

type family ErrorWhenNoValue
     (as :: [Assoc value])
     (k :: Key)
     (maybeV :: Maybe value)
  :: Constraint where

  ErrorWhenNoValue _ _ ('Just _)
    = ()
  ErrorWhenNoValue as k 'Nothing
    = TypeError
        (     'Text "The key "
        ':<>: 'Text k
        ':<>: 'Text " could not be found. The available keys are:"
        ':$$: AssocKeysError ('Text "  ") as
        )

type family AssocKeysError (prefix :: ErrorMessage) (as :: [Assoc value]) :: ErrorMessage where
  AssocKeysError prefix '[ '(k, v)]
    =     prefix
    ':<>: 'Text k
    ':<>: 'Text " :: "
    ':$$: 'ShowType v
  AssocKeysError prefix ('(k, v) ': as)
    =     prefix
    ':<>: 'Text k
    ':<>: 'Text " :: "
    ':$$: 'ShowType v
    ':$$: AssocKeysError prefix as

type family FromJust (a :: Maybe k) :: k where
  FromJust ('Just a)
    = a
