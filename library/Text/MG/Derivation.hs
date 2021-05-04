{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Text.MG.Derivation
    ( DerivationF(..)
    , Derivation
    , renderDerivation
    , treeDerivation
    ) where

import           Control.Recursion( Fix(..), cata )
import qualified Data.Text                 as T
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Tree                 as Tree
import           Text.MG.Grammar
import           Prelude.Unicode

data DerivationF f α β
    = Select (LexItem f α)
    | Merge1 β β
    | Merge2 β β
    | Merge3 β β
    | Move1  β
    | Move2  β
  deriving (Eq, Ord, Show, Functor)

type Derivation f α = Fix (DerivationF f α)

renderDerivation ∷ Derivation T.Text T.Text → T.Text
renderDerivation = cata renderAlg
  where
    renderAlg (Select li)   = renderLexItem li
    renderAlg (Merge1 x x') = "(Merge " <> x <> " " <> x' <> ")"
    renderAlg (Merge2 x x') = "(Merge " <> x <> " " <> x' <> ")"
    renderAlg (Merge3 x x') = "(Merge " <> x <> " " <> x' <> ")"
    renderAlg (Move1  x)    = "(Move "  <> x <> ")"
    renderAlg (Move2  x)    = "(Move "  <> x <> ")"

instance PP.Pretty α ⇒ PP.Pretty (Derivation f α) where
    pretty = cata prettyAlg
      where
        prettyAlg (Select (LexItem _ s))
                                = PP.parens $ "Select" PP.<+> PP.pretty s
        prettyAlg (Merge1 x x') = sexpr [ "Merge", x, x' ]
        prettyAlg (Merge2 x x') = sexpr [ "Merge", x, x' ]
        prettyAlg (Merge3 x x') = sexpr [ "Merge", x, x' ]
        prettyAlg (Move1  x)    = sexpr [ "Move",  x     ]
        prettyAlg (Move2  x)    = sexpr [ "Move",  x     ]

        sexpr = PP.parens ∘ PP.align ∘ PP.vsep

treeDerivation ∷ Derivation T.Text T.Text → Tree.Tree String
treeDerivation = cata treeAlg
  where
    treeAlg (Select li) = Tree.Node
        { Tree.rootLabel = T.unpack ∘ renderLexItem $ li
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
