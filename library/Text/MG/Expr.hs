{-# LANGUAGE DeriveFunctor #-}

module Text.MG.Expr
  ( Chain(..)
  , Expr(..)
  ) where

import Text.MG.Feature

import Data.Map (Map)

data Chain f β = Chain (FeatureStr f) β
  deriving (Eq, Ord, Show, Read, Functor)

-- Lexical?, main chain, movers
data Expr f β = Expr Bool (Chain f β) (Map f (Chain f β))
  deriving (Eq, Ord, Show, Read, Functor)
