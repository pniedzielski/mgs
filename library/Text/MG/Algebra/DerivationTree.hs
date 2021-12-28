{-# LANGUAGE FlexibleInstances #-}

module Text.MG.Algebra.DerivationTree
  ( derivationTree
  ) where

import Text.MG.Derivation ( Derivation, DerivationF(..) )
import Text.MG.Grammar ( lexItemContent )

import Data.Tree (Tree(..))
import Data.Comp


derivationTree ∷ Derivation f String → Tree String
derivationTree = cata derivationTreeAlg

class DerivationTree f where
    derivationTreeAlg ∷ Alg f (Tree String)

instance DerivationTree (DerivationF f String) where
    derivationTreeAlg (Select li)
        = Node { rootLabel = if null $ lexItemContent li
                             then "ε"
                             else lexItemContent li
               , subForest = []
               }
    derivationTreeAlg (Merge1 d1 d2)
        = Node { rootLabel = "Merge1"
               , subForest = [d1, d2]
               }
    derivationTreeAlg (Merge2 d1 d2)
        = Node { rootLabel = "Merge2"
               , subForest = [d1, d2]
               }
    derivationTreeAlg (Merge3 d1 d2)
        = Node { rootLabel = "Merge3"
               , subForest = [d1, d2]
               }
    derivationTreeAlg (Move1 d)
        = Node { rootLabel = "Move1"
               , subForest = [d]
               }
    derivationTreeAlg (Move2 d)
        = Node { rootLabel = "Move2"
               , subForest = [d]
               }
