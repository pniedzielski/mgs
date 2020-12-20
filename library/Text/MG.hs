{-# LANGUAGE DeriveFunctor #-}

module Text.MG
  ( Feature(..)
  , pos
  , FeatureStr
  , LexItem(..)
  , isEmptyLexItem
  , lexItemFeatures
  , Derivation(..)
  , Chain(..)
  , Expr(..)
  , Grammar(..)
  , emptyItems
  , valueItems
  , deriv2Tree
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tree (Tree(..))

-- Features are given structure in an MG.
data Feature f
    = Selectional f
    | Categorial f
    | Licenser f
    | Licensee f
  deriving (Eq, Ord, Show, Read, Functor)

pos :: Feature f -> Bool
pos (Selectional _) = True
pos (Categorial _)  = False
pos (Licenser _)    = True
pos (Licensee _)    = False

type FeatureStr f = NonEmpty (Feature f)

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
data Derivation f β
    = Select (LexItem f β)
    | Merge1 (Derivation f β) (Derivation f β)
    | Merge2 (Derivation f β) (Derivation f β)
    | Merge3 (Derivation f β) (Derivation f β)
    | Move1 (Derivation f β)
    | Move2 (Derivation f β)
  deriving (Eq, Show, Read, Functor)


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


deriv2Tree :: Derivation f String -> Tree String
deriv2Tree (Select li) = Node { rootLabel = if null $ lexItemContent li then "ε" else lexItemContent li
                              , subForest = []
                              }
deriv2Tree (Merge1 d1 d2) = Node { rootLabel = "Merge1"
                                 , subForest = [deriv2Tree d1, deriv2Tree d2]
                                 }
deriv2Tree (Merge2 d1 d2) = Node { rootLabel = "Merge2"
                                 , subForest = [deriv2Tree d1, deriv2Tree d2]
                                 }
deriv2Tree (Merge3 d1 d2) = Node { rootLabel = "Merge3"
                                 , subForest = [deriv2Tree d1, deriv2Tree d2]
                                 }
deriv2Tree (Move1 d) = Node { rootLabel = "Move1"
                            , subForest = [deriv2Tree d]
                            }
deriv2Tree (Move2 d) = Node { rootLabel = "Move2"
                            , subForest = [deriv2Tree d]
                            }
