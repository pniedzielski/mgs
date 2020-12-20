{-# LANGUAGE DeriveFunctor, FlexibleInstances #-}

module Text.MG
  ( module Text.MG.Feature
  , module Text.MG.Grammar
  , DerivationF(..)
  , Derivation
  , Chain(..)
  , Expr(..)
  , DerivationTree(..)
  ) where

import Text.MG.Feature
import Text.MG.Grammar
import Data.Map (Map)
import Data.Tree (Tree(..))
import Data.Comp

-- Derivation
data DerivationF f β α
    = Select (LexItem f β)
    | Merge1 α α
    | Merge2 α α
    | Merge3 α α
    | Move1  α
    | Move2  α
  deriving (Eq, Show, Read, Functor)


type Derivation f β = Term (DerivationF f β)


data Chain f β = Chain (FeatureStr f) β
  deriving (Eq, Ord, Show, Read, Functor)

-- Lexical?, main chain, movers
data Expr f β = Expr Bool (Chain f β) (Map f (Chain f β))
  deriving (Eq, Ord, Show, Read, Functor)


-------------------------------------------------------------------------------


class DerivationTree f where
    derivationTree :: Alg f (Tree String)

instance DerivationTree (DerivationF f String) where
    derivationTree (Select li)
        = Node { rootLabel = if null $ lexItemContent li then "ε" else lexItemContent li
               , subForest = []
               }
    derivationTree (Merge1 d1 d2)
        = Node { rootLabel = "Merge1"
               , subForest = [d1, d2]
               }
    derivationTree (Merge2 d1 d2)
        = Node { rootLabel = "Merge2"
               , subForest = [d1, d2]
               }
    derivationTree (Merge3 d1 d2)
        = Node { rootLabel = "Merge3"
               , subForest = [d1, d2]
               }
    derivationTree (Move1 d)
        = Node { rootLabel = "Move1"
               , subForest = [d]
               }
    derivationTree (Move2 d)
        = Node { rootLabel = "Move2"
               , subForest = [d]
               }
