{-# LANGUAGE DeriveFunctor #-}
module Main (main) where

import Data.List.NonEmpty( NonEmpty(..) )

main ∷ IO ()
main = do
  putStrLn "ᚣ"

data LexItem f α = LexItem (NonEmpty f) α
  deriving (Eq, Ord, Show, Functor)

data DerivationF f α β
    = Select (LexItem f α)
    | Merge1 β β
    | Merge2 β β
    | Merge3 β β
    | Move1  β
    | Move2  β
  deriving (Eq, Ord, Show, Functor)
