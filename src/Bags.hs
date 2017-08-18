{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Bags where

import           Data.Dynamic
import qualified Data.Map.Strict as M
import qualified Data.Text       as Tx
import           GHC.Generics
import           GHC.TypeLits

newtype Bag (fields :: [*])
  = Bag { _bagMap :: M.Map Tx.Text Dynamic }

data Field (name :: Symbol) (ty :: *)

class (KnownSymbol name, Typeable ty)
  =>  HasField (fields :: [*]) (name :: Symbol) (ty :: *) | fields name -> ty where

instance (KnownSymbol name, Typeable ty, Lookup fields fields name ~ ty)
      =>  HasField fields name ty

type family Lookup (allFields :: [*]) (fields :: [*]) (name :: Symbol) :: * where
  Lookup _ (Field name ty ': fields) name
    = ty
  Lookup allFields (field ': fields) name
    = Lookup allFields fields name
  Lookup '[] '[] name
    = TypeError ('Text "There are no available fields for this type")
  Lookup allFields '[] name
    = TypeError
        (     'Text "The field "
        ':<>: 'ShowType name
        ':<>: 'Text " could not be found. Please use one of:"
        ':$$: FieldNamesError allFields
        )

type family FieldNamesError (fields :: [*]) :: ErrorMessage where
  FieldNamesError '[Field name _]
    =     'Text "  "
    ':<>: 'ShowType name
  FieldNamesError (Field name _ ': fields)
    =     'Text "  "
    ':<>: 'ShowType name
    ':$$: FieldNamesError fields

symbolText :: forall s. KnownSymbol s => Tx.Text
symbolText
  = Tx.pack (symbolVal (Proxy @s))

empty :: Bag fields
empty
  = Bag M.empty

insert :: forall name fields ty. HasField fields name ty => ty -> Bag fields -> Bag fields
insert x (Bag m)
  = Bag (M.insert (symbolText @name) (toDyn x) m)

lookup :: forall name fields ty. HasField fields name ty => Bag fields -> Maybe ty
lookup (Bag m)
  = M.lookup (symbolText @name) m >>= fromDynamic

type ApplicationJourneyFields
  = '[ Field "MortgageAmount" MortgageAmount
     , Field "PropertyValue"  PropertyValue
     ]

newtype MortgageAmount
  = MortgageAmount { _mortgageAmountInt :: Int }
  deriving (Generic, Show)

newtype PropertyValue
  = PropertyValue { _propertyValueInt :: Int }
  deriving (Generic, Show)
