{-# LANGUAGE OverloadedStrings      #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Text.MG.IO
Description : Functionality to parse and serialize MG objects

Copyright   : © 2021, Patrick M. Niedzielski
License     : LGPL-3.0-or-later
Maintainer  : patrick@pniedzielski.net
Stability   : experimental
Portability : non-portable

This module provides functionality to parse Minimalist Grammars and
related objects from strings in a more conventional format, as well as
serialize them back to that same format.

For our output format, we choose to follow the format of Stabler's MG
CKY parser in SWI Prolog, available at
<https://linguistics.ucla.edu/people/stabler/coding.html>.  This
output format is in fact Prolog code, but it is still very readable.
-}

module Text.MG.IO
  ( -- * Formatting

    -- $formatting

    -- ** Feature
    formatFeature
  , formatFeatureWithFmt
    -- ** Feature String
  , formatFeatureStr
  , formatFeatureStrWithFmt
    -- ** Lexical Item
  , formatLexItem
  , formatLexItemWithFmt
    -- ** Grammar
  , formatGrammar
  , formatGrammarWithFmt

    -- * Parsing

    -- $parsing

    -- ** Feature
  , parseMG
  ) where

import Text.MG.Feature
import Text.MG.Grammar

import           Control.Applicative hiding (many, some)
import qualified Control.Applicative.Combinators.NonEmpty as NEComb
import           Control.Monad(void)
import           Data.Char(isLetter, isDigit)
import qualified Data.List.NonEmpty as NE
import           Data.Monoid.Unicode
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Void(Void)
import           Prelude.Unicode
import           Text.Megaparsec(Parsec, eof, many, choice, between, satisfy)
import           Text.Megaparsec.Char(space1, lowerChar)
import qualified Text.Megaparsec.Char.Lexer as L


-------------------------------------------------------------------------------
--                                                                FORMATTING --
-------------------------------------------------------------------------------


{-$formatting

_Formatting_ a Minimalist Grammar object is the process of taking the
object and turning it into a human-readable string representation.
Because the types in this library are polymorphic over the basic
feature set and over the content type of each lexical item, we provide
two variants for each formatting function in this module: one takes
the object with its basic feature set and/or content type already
formatted (i.e., instantiated to the @Data.Text.Text@ type), and one
takes the polymorphic object along with functions that will format the
basic feature set and/or content type.
-}


-------------------------------------------------------------------------------
--                                                            FORMAT FEATURE --
-------------------------------------------------------------------------------


-- | Format a @Feature f@ whose basic feature set has already been
--   formatted.
formatFeature :: Feature T.Text → T.Text
formatFeature = formatFeatureWithFmt id
{-# INLINABLE formatFeature #-}


-- | Format a @Feature f@ using a function that formats the basic
--   feature set @f@.
formatFeatureWithFmt ∷ (f → T.Text) → Feature f → T.Text
formatFeatureWithFmt fmt (Categorial  f) =       fmt f
formatFeatureWithFmt fmt (Selectional f) = "=" ⊕ fmt f
formatFeatureWithFmt fmt (Licensee    f) = "-" ⊕ fmt f
formatFeatureWithFmt fmt (Licenser    f) = "+" ⊕ fmt f


-------------------------------------------------------------------------------
--                                                     FORMAT FEATURE STRING --
-------------------------------------------------------------------------------


-- | Format a @FeatureStr f@ whose basic feature set has already been
--   formatted.
formatFeatureStr ∷ FeatureStr T.Text → T.Text
formatFeatureStr = formatFeatureStrWithFmt id
{-# INLINABLE formatFeatureStr #-}


-- | Format a @FeatureStr f@ using a function that formats the basic
--   feature set @f@.
formatFeatureStrWithFmt ∷ (f → T.Text) → FeatureStr f → T.Text
formatFeatureStrWithFmt fmt
    = T.intercalate "," ∘ NE.toList ∘ fmap (formatFeatureWithFmt fmt)


-------------------------------------------------------------------------------
--                                                       FORMAT LEXICAL ITEM --
-------------------------------------------------------------------------------


-- | Format a @LexItem f β@ whose basic feature set and content type
--   have already been formatted.
formatLexItem ∷ LexItem T.Text T.Text → T.Text
formatLexItem = formatLexItemWithFmt id id
{-# INLINABLE formatLexItem #-}


-- | Format a @LexItem f β@ using a function that formats the basic
--   feature set @f@ along with a function that formats the content
--   type @β@.
formatLexItemWithFmt
    ∷ (f → T.Text)
    → (β → T.Text)
    → LexItem f β
    → T.Text
formatLexItemWithFmt fFmt βFmt (LexItem fs β)
    = "[" ⊕ βFmt β ⊕ "] ∷ [" ⊕ formatFeatureStrWithFmt fFmt fs ⊕ "]"


-------------------------------------------------------------------------------
--                                                            FORMAT GRAMMAR --
-------------------------------------------------------------------------------


-- | Format a @Grammar f β@ whose basic feature set and content type
--   have already been formatted.
formatGrammar ∷ Grammar T.Text T.Text → T.Text
formatGrammar = formatGrammarWithFmt id id
{-# INLINABLE formatGrammar #-}


-- | Format a @Grammar f β@ using a function that formats the basic
--   feature set @f@ along with a function that formats the content
--   type @β@.
formatGrammarWithFmt
    ∷ (f → T.Text)
    → (β → T.Text)
    → Grammar f β
    → T.Text
formatGrammarWithFmt fFmt βFmt g
    = lexiconStr ⊕ "\n" ⊕ startCategoryStr
  where
    lexiconStr = T.intercalate " "
               ∘ fmap (\li → formatLexItemWithFmt fFmt βFmt li ⊕ ".")
               ∘ S.toList
               $ lexicon g
    startCategoryStr = "startCategory("
                     ⊕ fFmt (startCategory g)
                     ⊕ ").\n"


-------------------------------------------------------------------------------
--                                                                   PARSING --
-------------------------------------------------------------------------------


{-$parsing

_Formatting_ a Minimalist Grammar object is the process of taking the
object and turning it into a human-readable string representation.
Because the types in this library are polymorphic over the basic
feature set and over the content type of each lexical item, we provide
two variants for each formatting function in this module: one takes
the object with its basic feature set and/or content type already
formatted (i.e., instantiated to the @Data.Text.Text@ type), and one
takes the polymorphic object along with functions that will format the
basic feature set and/or content type.
-}


-------------------------------------------------------------------------------
--                                                             PARSE FEATURE --
-------------------------------------------------------------------------------

type Parser α = Parsec Void T.Text α

sc ∷ Parser ()
sc = L.space
    space1
    (L.skipLineComment "%")
    (L.skipBlockComment "/*" "*/")

lexeme ∷ Parser α → Parser α
lexeme = L.lexeme sc

symbol ∷ T.Text → Parser ()
symbol = void ∘ L.symbol sc

equals, plus, minus, comma, period, lBracket, rBracket, lParen, rParen, quote, sep ∷ Parser ()
equals   = symbol "="
plus     = symbol "+"
minus    = symbol "-"
comma    = symbol ","
period   = symbol "."
lBracket = symbol "["
rBracket = symbol "]"
lParen   = symbol "("
rParen   = symbol ")"
quote    = symbol "'"
sep      = symbol "∷" <|> symbol "::"

startCatToken ∷ Parser ()
startCatToken = symbol "startCategory"

quoted, bracketed, parened ∷ Parser α → Parser α
quoted    = between quote quote
bracketed = between lBracket rBracket
parened   = between lParen rParen

character ∷ Parser Char
character = satisfy $
    \s → isLetter s
       ∨ isDigit s
       ∨ s ≡ '_'
       ∨ s ≡ '+'
       ∨ s ≡ '-'
       ∨ s ≡ '*'
       ∨ s ≡ '/'
       ∨ s ≡ '\\'
       ∨ s ≡ '^'
       ∨ s ≡ '~'
       ∨ s ≡ ':'
       ∨ s ≡ '.'
       ∨ s ≡ '?'
       ∨ s ≡ '#'
       ∨ s ≡ '$'
       ∨ s ≡ '&'

simpleCharacter ∷ Parser Char
simpleCharacter = satisfy $
    \s → isLetter s
       ∨ isDigit s
       ∨ s ≡ '_'

atom ∷ Parser T.Text
atom = T.pack <$>
    (lexeme $ (simpleAtom <|> stringAtom))
  where
    simpleAtom = (:) <$> lowerChar <*> (many simpleCharacter)
    stringAtom = quoted (many character)

parseFeature ∷ Parser (Feature T.Text)
parseFeature = featureType <*> basicFeature

basicFeature ∷ Parser (T.Text)
basicFeature = atom

featureType ∷ Parser (T.Text → Feature T.Text)
featureType =
    choice [ Selectional <$ equals
           , Licenser    <$ plus
           , Licensee    <$ minus
           , return Categorial
           ]

parseFeatureStr ∷ Parser (FeatureStr T.Text)
parseFeatureStr = parseFeature `NEComb.sepBy1` comma

parseLexItem ∷ Parser (LexItem T.Text T.Text)
parseLexItem = do
    content ← bracketed atom
    ()      ← sep
    fs      ← bracketed parseFeatureStr
    return $ LexItem fs content

parseGrammarStatement ∷ Parser α → Parser α
parseGrammarStatement p = p <* period

parseStartCat ∷ Parser (T.Text)
parseStartCat = startCatToken *> (parened basicFeature)

parseGrammar ∷ Parser (Grammar T.Text T.Text)
parseGrammar = do
    lex1 ← many ∘ parseGrammarStatement $ parseLexItem
    c    ← parseGrammarStatement parseStartCat
    lex2 ← many ∘ parseGrammarStatement $ parseLexItem
    return $
        Grammar { startCategory = c
                , lexicon = S.fromList lex1 `S.union` S.fromList lex2
                }

parseMG ∷ Parser (Grammar T.Text T.Text)
parseMG = sc *> parseGrammar <* sc <* eof
