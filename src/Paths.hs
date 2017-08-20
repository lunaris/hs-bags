{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Paths
  ( Path (..)
  , type (//)

  , PathNames
  , pathText
  ) where

import All

import qualified Data.Text    as Tx
import           GHC.TypeLits

data Path name
  = Leaf name
  | Node name (Path name)

type family (//) (name :: Symbol) (path :: k2) :: Path Symbol where
  name // leaf
    = 'Node name ('Leaf leaf)
  name // path
    = 'Node name path
  name // path
    = TypeError
        (     'Text "The type "
        ':<>: 'ShowType path
        ':<>: 'Text " is neither a Symbol nor a Path Symbol"
        )

infixr 5 //

type family PathNames (path :: k) :: [Symbol] where
  PathNames name
    = '[name]
  PathNames ('Leaf name)
    = '[name]
  PathNames ('Node name path)
    = name ': PathNames path
  PathNames path
    = TypeError
        (     'Text "The type "
        ':<>: 'ShowType path
        ':<>: 'Text " is neither a Symbol nor a Path Symbol"
        )

pathText :: forall path. All KnownSymbol (PathNames path) => Tx.Text
pathText
  = Tx.intercalate "/" $
      withAll @_ @KnownSymbol @(PathNames path) symbolText

symbolText :: KnownSymbol s => proxy s -> Tx.Text
symbolText
  = Tx.pack . symbolVal
