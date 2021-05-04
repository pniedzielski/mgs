{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE OverloadedStrings    #-}

module Text.MG.Grammar
    ( LexItem(..)
    , renderLexItem
    ) where

import           Data.Foldable( fold )
import           Data.List.NonEmpty( NonEmpty(..) )
import qualified Data.List.NonEmpty        as NonEmpty
import qualified Data.Text                 as T
import           Text.MG.Feature
import           Prelude.Unicode

data LexItem f α = LexItem (NonEmpty (Feature f)) α
  deriving (Eq, Ord, Show, Functor)

renderLexItem ∷ LexItem T.Text T.Text → T.Text
renderLexItem (LexItem fs α) = "[" <> α <> " ∷ " <> fStr <> "]"
  where
    fStr = fold ∘ NonEmpty.intersperse " " $ renderFeature <$> fs
