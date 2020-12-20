{-# LANGUAGE DeriveFunctor, FlexibleInstances #-}

module Text.MG
  ( module Text.MG.Feature
  , LexItem(..)
  , isEmptyLexItem
  , lexItemFeatures
  , DerivationF(..)
  , Derivation
  , Chain(..)
  , Expr(..)
  , Grammar(..)
  , emptyItems
  , valueItems
  , DerivationTree(..)
  ) where

import Text.MG.Feature
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tree (Tree(..))
import Data.Comp

-- Lexical item over syntactic features `Feature f` and nonsyntactic
-- features α.
data LexItem f β = LexItem (FeatureStr f) β
  deriving (Eq, Ord, Show, Read, Functor)

isEmptyLexItem :: LexItem f String -> Bool
isEmptyLexItem (LexItem _ β) = null β

lexItemFeatures :: LexItem f β -> FeatureStr f
lexItemFeatures (LexItem fs _) = fs

lexItemContent :: LexItem f β -> β
lexItemContent (LexItem _ β) = β

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

-- MGs are lexicalized formalisms
data Grammar f β = Grammar
    { startCategory :: f
    , lexicon :: Set (LexItem f β)
    }
  deriving (Eq, Ord, Show, Read)

emptyItems :: Grammar f String -> [LexItem f String]
emptyItems = Set.toList . Set.filter isEmptyLexItem . lexicon

valueItems :: Eq β =>  Grammar f β -> β -> [LexItem f β]
valueItems g β = (Set.toList . Set.filter (\x -> lexItemContent x == β) . lexicon) g


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
