{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE UndecidableInstances  #-}

module JSON
  ( bagToJSON
  ) where

import All
import Assocs
import Types

import           Data.Aeson
import           Data.Dynamic
import           Data.Kind
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid     (First(..))
import qualified Data.Text       as Tx
import           GHC.TypeLits

class (KnownSymbol (Fst a),
       Typeable (f (Snd a)),
       ToJSON (f (Snd a)))

    => AssocToJSON (f :: Type -> Type) (a :: Assoc Type) where

  assocValue :: Tx.Text -> Dynamic -> Proxy a -> First Value

instance (KnownSymbol (Fst a),
          Typeable (f (Snd a)),
          ToJSON (f (Snd a)))

      =>  AssocToJSON (f :: Type -> Type) (a :: Assoc Type) where

  assocValue k dyn _
    | keyText @(Fst a) == k
        = First (toJSON @(f (Snd a)) <$> fromDynamic dyn)
    | otherwise
        = First Nothing

bagToJSON
  :: forall as f.
     All (AssocToJSON f) as
  => Bag f as
  -> Value

bagToJSON (Bag m)
  = toJSON $ M.mapWithKey f m
  where
    f :: Tx.Text -> Dynamic -> Value
    f k dyn
      = fromMaybe (error "bagToJSON: Impossible")
      $ getFirst
      $ withAll @_ @(AssocToJSON f) @as (assocValue @f k dyn)
