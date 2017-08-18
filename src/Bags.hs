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
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}

module Bags where

import           Data.Dynamic
import qualified Data.Map.Strict as M
import qualified Data.Text       as Tx
import           GHC.Exts
import           GHC.TypeLits

newtype Bag (fields :: [*])
  = Bag { _bagMap :: M.Map Tx.Text Dynamic }

data Path name
  = Leaf name
  | Node name (Path name)

type family (//) (name :: k1) (path :: k2) :: Path Symbol where
  name // leaf
    = 'Node name ('Leaf leaf)
  name // path
    = 'Node name path

type family PathNames (path :: k) :: [Symbol] where
  PathNames name
    = '[name]
  PathNames ('Leaf name)
    = '[name]
  PathNames ('Node name path)
    = name ': PathNames path

data Field (name :: Symbol) ty

type family All (c :: k -> Constraint) (as :: [k]) :: Constraint where
  All c '[]       = ()
  All c (a ': as) = (c a, All c as)

class (All KnownSymbol (PathNames path), Typeable ty)
  =>  HasField (fields :: k1) (path :: k2) (ty :: *) | fields path -> ty where

instance (All KnownSymbol (PathNames path),
          Typeable ty,
          Lookup fields path ~ maybeTy,
          maybeTy ~ 'Just ty,
          ErrorWhenNothing path maybeTy)

      =>  HasField (fields :: k1) (path :: k2) (ty :: *) where

type family Lookup (fields :: k1) (path :: k2) :: Maybe * where
  Lookup (Field name fields ': _) ('Node name path)
    = Lookup fields path
  Lookup (Field name ty ': _) ('Leaf name)
    = 'Just ty
  Lookup (Field name ty ': _) name
    = 'Just ty
  Lookup (_ ': fields) path
    = Lookup fields path
  Lookup '[] _
    = 'Nothing

type family ErrorWhenNothing (path :: k) (maybeTy :: Maybe *) :: Constraint where
  ErrorWhenNothing _ ('Just _)
    = ()
  ErrorWhenNothing path 'Nothing
    = TypeError
        (     'Text "The field "
        ':<>: 'ShowType path
        ':<>: 'Text " could not be found."
        )

empty :: Bag fields
empty
  = Bag M.empty

insert :: forall name fields ty. HasField fields name ty => ty -> Bag fields -> Bag fields
insert --x (Bag m)
  = undefined -- Bag (M.insert (symbolText @name) (toDyn x) m)

lookup :: forall name fields ty. HasField fields name ty => Bag fields -> Maybe ty
lookup --(Bag m)
  = undefined -- M.lookup (symbolText @name) m >>= fromDynamic

{-
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
  -}

type ApplicationJourneyFields
  = '[ Field "MortgageAmount" MortgageAmount
     , Field "PropertyValue"  PropertyValue
     , Field "Transaction"    TransactionFields
     ]

type TransactionFields
  = '[ Field "MortgageAmount" MortgageAmount
     , Field "PropertyValue"  PropertyValue
     ]

newtype MortgageAmount
  = MortgageAmount { _mortgageAmountInt :: Int }
  deriving (Show)

newtype PropertyValue
  = PropertyValue { _propertyValueInt :: Int }
  deriving (Show)
