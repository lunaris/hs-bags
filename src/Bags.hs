{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bags
  ( module Exports

  , module Bags
  ) where

import Operations as Exports
import JSON       as Exports
import Types      as Exports

import Data.Aeson

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
  deriving (Show, ToJSON)

newtype PropertyValue
  = PropertyValue { _propertyValueInt :: Int }
  deriving (Show, ToJSON)
