{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE OverloadedStrings    #-}

module Text.MG.Feature
  ( -- * Feature type
    Feature(..)
  , basicFeature
    -- * Polarity
  , Polarity(..)
  , polarity
  , isPositive
  , isNegative
    -- * Operation
  , Op(..)
  , operation
  , isMergeable
  , isMoveable
    -- * Annihilation
  , annihilates
  , opposite
    -- * Rendering
  , renderFeature
    -- * Parsing
  , parseFeature
  , parseFeatureList
  ) where

import           Data.List.NonEmpty hiding( cons )
import           Data.Text( Text, pack, cons )
import           Data.Void
import           Prelude.Unicode
import           Text.Megaparsec hiding( State )
import           Text.Megaparsec.Char( alphaNumChar, char, letterChar, space, space1 )

data Feature f
    = Categorial f
    | Selectional f
    | Licenser f
    | Licensee f
  deriving (Eq, Ord, Show, Functor)

basicFeature ∷ Feature f → f
basicFeature (Categorial f)  = f
basicFeature (Selectional f) = f
basicFeature (Licenser f)    = f
basicFeature (Licensee f)    = f

data Polarity = Pos | Neg
  deriving (Eq, Ord, Show)

polarity ∷ Feature f → Polarity
polarity (Categorial _)  = Neg
polarity (Selectional _) = Pos
polarity (Licenser _)    = Pos
polarity (Licensee _)    = Neg

isPositive ∷ Feature f → Bool
isPositive = (≡ Pos) ∘ polarity

isNegative ∷ Feature f → Bool
isNegative = (≡ Neg) ∘ polarity

data Op = OpMerge | OpMove
  deriving (Eq, Ord, Show)

operation ∷ Feature f → Op
operation (Categorial _)  = OpMerge
operation (Selectional _) = OpMerge
operation (Licenser _)    = OpMove
operation (Licensee _)    = OpMove

isMergeable ∷ Feature f → Bool
isMergeable = (≡ OpMerge) ∘ operation

isMoveable ∷ Feature f → Bool
isMoveable = (≡ OpMove) ∘ operation

annihilates ∷ Eq f ⇒ Feature f → Feature f → Bool
annihilates f f'
    = polarity f     ≢ polarity f'
    ∧ operation f    ≡ operation f'
    ∧ basicFeature f ≡ basicFeature f'

opposite ∷ Feature f → Feature f
opposite (Categorial f)  = Selectional f
opposite (Selectional f) = Categorial f
opposite (Licenser f)    = Licensee f
opposite (Licensee f)    = Licenser f

renderFeature ∷ Feature Text → Text
renderFeature (Categorial f)  = f
renderFeature (Selectional f) = "=" <> f
renderFeature (Licenser f)    = "+" <> f
renderFeature (Licensee f)    = "-" <> f

parseFeature ∷ Parsec Void Text (Feature Text)
parseFeature = choice
    [              Categorial  <$> identifier
    , char '~' *> (Categorial  <$> identifier)
    , char '=' *> (Selectional <$> identifier)
    , char '+' *> (Licenser    <$> identifier)
    , char '-' *> (Licensee    <$> identifier)
    ]
  where
    identifier = (cons) <$> letterChar
                        <*> (pack <$> many (alphaNumChar <|> char '_'))

parseFeatureList ∷ Parsec Void Text (NonEmpty (Feature Text))
parseFeatureList = fromList <$> sepBy1 parseFeature space1 <* space
