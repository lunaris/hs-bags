{-# LANGUAGE DataKinds #-}

module Bags
  ( module Exports

  , module Bags
  ) where

import Operations as Exports
import Types      as Exports

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
