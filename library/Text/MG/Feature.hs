{-# LANGUAGE DeriveFunctor #-}

module Text.MG.Feature
  ( Feature(..)
  , pos
  , neg
  , FeatureStr
  ) where

import Data.List.NonEmpty (NonEmpty)

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

neg :: Feature f -> Bool
neg = not . pos

type FeatureStr f = NonEmpty (Feature f)
