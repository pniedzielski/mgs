{-# LANGUAGE DeriveFunctor #-}

module Text.MG.Expr
  ( Chain(..)
  , Expr(..)
  , ExprType(..)
  ) where

import Text.MG.Feature

import Data.Map (Map)

data Chain f β = Chain (FeatureStr f) β
  deriving (Eq, Ord, Show, Read, Functor)

data ExprType = SimplexExpr | ComplexExpr
  deriving (Eq, Ord, Show, Read)

-- Lexical?, main chain, movers
data Expr f β = Expr ExprType (Chain f β) (Map f (Chain f β))
  deriving (Eq, Ord, Show, Read, Functor)
