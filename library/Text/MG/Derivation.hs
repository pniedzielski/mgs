{-# LANGUAGE DeriveFunctor, FlexibleInstances #-}

module Text.MG.Derivation
  ( DerivationF(..)
  , Derivation
  , DerivationTree(..)
  ) where

import Text.MG.Grammar

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
  deriving (Eq, Ord, Show, Read, Functor)

type Derivation f β = Term (DerivationF f β)


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
