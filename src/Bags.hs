module Bags where

import qualified Data.Dynamic    as Dyn
import qualified Data.Map.Strict as M
import qualified Data.Text       as Tx
import qualified GHC.Generics    as G

newtype Bag field
  = Bag { _bagMap :: M.Map Tx.Text Dyn.Dynamic }

class Dyn.Typeable field => Field field where
  keyOf :: field -> Tx.Text

empty :: Bag field
empty
  = Bag M.empty

insert :: Field field => field -> Bag field -> Bag field
insert x (Bag bag)
  = Bag (M.insert (keyOf x) (Dyn.toDyn x) bag)

lookup :: (a -> field) -> Bag field -> Maybe a
lookup
  = undefined

data ApplicationJourneyField
  = AMortgageAmount MortgageAmount
  | APropertyValue PropertyValue
  deriving (G.Generic)

instance Field ApplicationJourneyField where
  keyOf
    = \case
        AMortgageAmount _ -> "MortgageAmount"
        APropertyValue _  -> "PropertyValue"

newtype MortgageAmount
  = MortgageAmount { _mortgageAmountInt :: Int }
  deriving (G.Generic)

newtype PropertyValue
  = PropertyValue { _propertyValueInt :: Int }
  deriving (G.Generic)
