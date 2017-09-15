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

class (All KnownSymbol (PathNames (Fst keyAssoc)),
       Typeable (Snd keyAssoc),
       ToJSON (Snd keyAssoc))

    => KeyAssocToJSON (keyAssoc :: (Path, *)) where

  keyAssocValue :: Tx.Text -> Dynamic -> Proxy keyAssoc -> First Value

instance (All KnownSymbol (PathNames (Fst keyAssoc)),
          Typeable (Snd keyAssoc),
          ToJSON (Snd keyAssoc))

      =>  KeyAssocToJSON (keyAssoc :: (Path, *)) where

  keyAssocValue k dyn _
    | pathText @(Fst keyAssoc) == k
        = First (toJSON @(Snd keyAssoc) <$> fromDynamic dyn)
    | otherwise
        = First Nothing

bagToJSON
  :: forall keys f.
     All KeyAssocToJSON (KeyAssocs f keys)
  => Bag f keys
  -> Value

bagToJSON (Bag m)
  = toJSON $ M.mapWithKey f m
  where
    f :: Tx.Text -> Dynamic -> Value
    f k dyn
      = fromMaybe (error "bagToJSON: Impossible")
      $ getFirst
      $ withAll @_ @KeyAssocToJSON @(KeyAssocs f keys) (keyAssocValue k dyn)
