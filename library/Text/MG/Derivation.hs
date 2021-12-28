{-# LANGUAGE DeriveTraversable #-}

module Text.MG.Derivation
  ( DerivationF(..)
  , Derivation
  ) where

import Text.MG.Grammar

import Data.Comp

-- Derivation
data DerivationF f β α
    = Select (LexItem f β)
    | Merge1 α α
    | Merge2 α α
    | Merge3 α α
    | Move1  α
    | Move2  α
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

type Derivation f β = Term (DerivationF f β)
