{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE OverloadedStrings    #-}

module Text.MG.Grammar
    ( LexItem(..)
    , renderLexItem
    , parseLexItem
    ) where

import           Control.Monad( void )
import           Data.Foldable( fold )
import           Data.List.NonEmpty( NonEmpty(..) )
import qualified Data.List.NonEmpty        as NonEmpty
import qualified Data.Text                 as T
import           Data.Void
import           Text.Megaparsec hiding( State )
import           Text.Megaparsec.Char( letterChar, hspace, string )
import           Text.MG.Feature
import           Prelude.Unicode

data LexItem f α = LexItem (NonEmpty (Feature f)) α
  deriving (Eq, Ord, Show, Functor)

renderLexItem ∷ LexItem T.Text T.Text → T.Text
renderLexItem (LexItem fs α) = α <> " ∷ " <> fStr
  where
    fStr = fold ∘ NonEmpty.intersperse " " $ renderFeature <$> fs

parseLexItem ∷ Parsec Void T.Text (LexItem T.Text T.Text)
parseLexItem =
  do
    identifier ← T.pack <$> many letterChar
    void separator
    features ← parseFeatureList
    return $ LexItem features identifier
  where
    separator = hspace *> (string "∷" <|> string "::") <* hspace
