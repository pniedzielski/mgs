{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main (main) where

import           Control.Recursion( Fix(..), cata )
import           Data.List.NonEmpty( NonEmpty(..) )
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import qualified Data.Text.Prettyprint.Doc as PP
import           Data.Text.Prettyprint.Doc.Render.Text( putDoc )
import qualified Data.Tree                 as Tree
import           Prelude.Unicode

main ∷ IO ()
main = do
  TIO.putStrLn ∘ cata renderAlg $ deriv3
  putDoc ∘ PP.pretty $ deriv4
  TIO.putStrLn ""
  putStrLn ∘ Tree.drawTree ∘ cata treeAlg $ deriv4

data LexItem f α = LexItem (NonEmpty f) α
  deriving (Eq, Ord, Show, Functor)

li1, li2 ∷ LexItem Char T.Text
li1 = LexItem ('a' :| ['b']) "a"
li2 = LexItem ('c' :| []) "b"

data DerivationF f α β
    = Select (LexItem f α)
    | Merge1 β β
    | Merge2 β β
    | Merge3 β β
    | Move1  β
    | Move2  β
  deriving (Eq, Ord, Show, Functor)

type Derivation f α = Fix (DerivationF f α)

deriv1, deriv2, deriv3, deriv4 ∷ Derivation Char T.Text
deriv1 = Fix (Select li1)
deriv2 = Fix (Select li2)
deriv3 = Fix (Merge1 deriv1 deriv2)
deriv4 = Fix (Merge2 deriv3 (Fix (Merge2 deriv3 deriv3)))

renderAlg ∷ DerivationF f T.Text T.Text → T.Text
renderAlg (Select (LexItem _ s)) = s
renderAlg (Merge1 x x') = "(Merge " <> x <> " " <> x' <> ")"
renderAlg (Merge2 x x') = "(Merge " <> x <> " " <> x' <> ")"
renderAlg (Merge3 x x') = "(Merge " <> x <> " " <> x' <> ")"
renderAlg (Move1  x)    = "(Move "  <> x <> ")"
renderAlg (Move2  x)    = "(Move "  <> x <> ")"

instance PP.Pretty α ⇒ PP.Pretty (Derivation f α) where
    pretty = cata prettyAlg

prettyAlg ∷ PP.Pretty α ⇒ DerivationF f α (PP.Doc ann) → (PP.Doc ann)
prettyAlg (Select (LexItem _ s))
                        = PP.parens $ "Select" PP.<+> PP.pretty s
prettyAlg (Merge1 x x') = sexpr [ "Merge", x, x' ]
prettyAlg (Merge2 x x') = sexpr [ "Merge", x, x' ]
prettyAlg (Merge3 x x') = sexpr [ "Merge", x, x' ]
prettyAlg (Move1  x)    = sexpr [ "Move",  x     ]
prettyAlg (Move2  x)    = sexpr [ "Move",  x     ]

sexpr ∷ [PP.Doc ann] → PP.Doc ann
sexpr = PP.parens ∘ PP.align ∘ PP.vsep

treeAlg ∷ DerivationF f T.Text (Tree.Tree String) → Tree.Tree String
treeAlg (Select (LexItem _ s)) = Tree.Node
    { Tree.rootLabel = T.unpack s
    , Tree.subForest = []
    }
treeAlg (Merge1 x x') = Tree.Node
    { Tree.rootLabel = "Merge"
    , Tree.subForest = [x, x']
    }
treeAlg (Merge2 x x') = Tree.Node
    { Tree.rootLabel = "Merge"
    , Tree.subForest = [x, x']
    }
treeAlg (Merge3 x x') = Tree.Node
    { Tree.rootLabel = "Merge"
    , Tree.subForest = [x, x']
    }
treeAlg (Move1 x) = Tree.Node
    { Tree.rootLabel = "Move"
    , Tree.subForest = [x]
    }
treeAlg (Move2 x) = Tree.Node
    { Tree.rootLabel = "Move"
    , Tree.subForest = [x]
    }
