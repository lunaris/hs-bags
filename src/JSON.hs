{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module JSON
  ( bagToJSON
  ) where

import All
import Paths
import Types

import           Data.Aeson
import           Data.Dynamic
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid     (First(..))
import qualified Data.Text       as Tx
import           GHC.TypeLits

type family Fst (t :: (a, b)) :: a where
  Fst '(a, b)
    = a

type family Snd (t :: (a, b)) :: b where
  Snd '(a, b)
    = b

class (All KnownSymbol (PathNames (Fst fieldAssoc)),
       Typeable (Snd fieldAssoc),
       ToJSON (Snd fieldAssoc))

    => FieldAssocToJSON (fieldAssoc :: (Path Symbol, *)) where

  fieldAssocValue :: Tx.Text -> Dynamic -> Proxy fieldAssoc -> First Value

instance (All KnownSymbol (PathNames (Fst fieldAssoc)),
          Typeable (Snd fieldAssoc),
          ToJSON (Snd fieldAssoc))

      =>  FieldAssocToJSON (fieldAssoc :: (Path Symbol, *)) where

  fieldAssocValue k dyn _
    | pathText @(Fst fieldAssoc) == k
        = First (toJSON @(Snd fieldAssoc) <$> fromDynamic dyn)
    | otherwise
        = First Nothing

bagToJSON
  :: forall fields.
     All FieldAssocToJSON (FieldAssocs fields)
  => Bag fields
  -> Value

bagToJSON (Bag m)
  = toJSON $ M.mapWithKey f m
  where
    f :: Tx.Text -> Dynamic -> Value
    f k dyn
      = fromMaybe (error "bagToJSON: Impossible")
      $ getFirst
      $ withAll @_ @FieldAssocToJSON @(FieldAssocs fields) (fieldAssocValue k dyn)
