{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Paths
  ( Path (..)
  , p
  , ps

  , PathNames
  , NamesPath
  , pathText
  ) where

import All

import qualified Data.Text                 as Tx
import           GHC.TypeLits
import qualified Language.Haskell.TH       as TH
import qualified Language.Haskell.TH.Quote as TH.Q

data Path
  = Leaf Symbol
  | Node Symbol Path

p :: TH.Q.QuasiQuoter
p
  = TH.Q.QuasiQuoter
      { quoteExp  = fail "The [p| ... |] quasiquoter cannot be used in an expression context"
      , quotePat  = fail "The [p| ... |] quasiquoter cannot be used in a pattern context"
      , quoteType = quoteTypeP
      , quoteDec  = fail "The [p| ... |] quasiquoter cannot be used in a declaration context"
      }

quoteTypeP :: String -> TH.TypeQ
quoteTypeP
  = foldr1Splits nodeQ leafQ '/' . trim
  where
    symbolQ
      = TH.litT . TH.strTyLit
    leafQ name
      = TH.appT (TH.conT 'Leaf) (symbolQ name)
    nodeQ name
      = TH.appT (TH.appT (TH.conT 'Node) (symbolQ name))

ps :: TH.Q.QuasiQuoter
ps
  = TH.Q.QuasiQuoter
      { quoteExp  = fail "The [ps| ... |] quasiquoter cannot be used in an expression context"
      , quotePat  = fail "The [ps| ... |] quasiquoter cannot be used in a pattern context"
      , quoteType = quoteTypePs
      , quoteDec  = fail "The [ps| ... |] quasiquoter cannot be used in a declaration context"
      }

quoteTypePs :: String -> TH.TypeQ
quoteTypePs
  = foldr1Splits consQ singQ ',' . trim
  where
    singQ path
      = TH.appT (TH.appT TH.promotedConsT (pathQ path)) TH.promotedNilT
    consQ path
      = TH.appT (TH.appT TH.promotedConsT (pathQ path))

    pathQ
      = foldr1Splits nodeQ leafQ '/' . trim

    symbolQ
      = TH.litT . TH.strTyLit
    leafQ name
      = TH.appT (TH.conT 'Leaf) (symbolQ name)
    nodeQ name
      = TH.appT (TH.appT (TH.conT 'Node) (symbolQ name))

trim :: String -> String
trim
  = k . k
  where
    k = reverse . dropWhile (== ' ')

foldr1Splits :: (String -> a -> a) -> (String -> a) -> Char -> String -> a
foldr1Splits fInits fLast c
  = foldr1Splits' . breakOnSep
  where
    breakOnSep
      = break (== c)
    dropSep
      = drop 1

    foldr1Splits' (s1, s2)
      | null s2   = fLast s1
      | otherwise = fInits s1 (foldr1Splits' (breakOnSep (dropSep s2)))

type family PathNames (path :: Path) :: [Symbol] where
  PathNames ('Leaf name)
    = '[name]
  PathNames ('Node name path)
    = name ': PathNames path

type family NamesPath (names :: [Symbol]) :: Path where
  NamesPath '[name]
    = 'Leaf name
  NamesPath (name ': names)
    = 'Node name (NamesPath names)
  NamesPath '[]
    = TypeError ('Text "Cannot create an empty path")

pathText :: forall path. All KnownSymbol (PathNames path) => Tx.Text
pathText
  = Tx.intercalate "/" $
      withAll @_ @KnownSymbol @(PathNames path) symbolText

symbolText :: KnownSymbol s => proxy s -> [Tx.Text]
symbolText
  = (:[]) . Tx.pack . symbolVal
