{-# LANGUAGE DeriveFunctor #-}

module Text.MG.Grammar
  ( LexItem(..)
  , isEmptyLexItem
  , lexItemFeatures
  , lexItemContent
  , Grammar(..)
  , emptyItems
  , valueItems
  ) where

import Text.MG.Feature

import Data.Set (Set)
import qualified Data.Set as Set

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
