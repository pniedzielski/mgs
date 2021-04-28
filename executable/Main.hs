{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Recursion( Fix(..), cata )
import           Data.List.NonEmpty( NonEmpty(..) )
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import           Prelude.Unicode

main ∷ IO ()
main = do
  TIO.putStrLn ∘ cata renderAlg $ deriv3

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

type Derivation f α = Fix (DerivationF f α)

deriv1, deriv2, deriv3 ∷ Derivation Char T.Text
deriv1 = Fix (Select (LexItem ('a' :| ['b']) "a"))
deriv2 = Fix (Select (LexItem ('c' :| []) "b"))
deriv3 = Fix (Merge1 deriv1 deriv2)

renderAlg ∷ DerivationF f T.Text T.Text → T.Text
renderAlg (Select (LexItem _ s)) = s
renderAlg (Merge1 x x') = "(Merge " <> x <> " " <> x' <> ")"
renderAlg (Merge2 x x') = "(Merge " <> x <> " " <> x' <> ")"
renderAlg (Merge3 x x') = "(Merge " <> x <> " " <> x' <> ")"
renderAlg (Move1  x)    = "(Move "  <> x <> ")"
renderAlg (Move2  x)    = "(Move "  <> x <> ")"
