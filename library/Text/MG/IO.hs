{-# LANGUAGE OverloadedStrings #-}
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
  ) where

import qualified Data.Text as T
import           Prelude.Unicode
import qualified Data.List.NonEmpty as NE
import           Data.Monoid.Unicode
import qualified Data.Set as S

import Text.MG.Feature( Feature(..), FeatureStr )
import Text.MG.Grammar( LexItem(..), lexItemFeatures, lexItemContent, Grammar(..) )


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
formatFeature (Categorial  f) =       f
formatFeature (Selectional f) = "=" ⊕ f
formatFeature (Licensee    f) = "-" ⊕ f
formatFeature (Licenser    f) = "+" ⊕ f


-- | Format a @Feature f@ using a function that formats the basic
--   feature set @f@.
formatFeatureWithFmt ∷ (f → T.Text) → Feature f → T.Text
formatFeatureWithFmt fmt = formatFeature ∘ fmap fmt


-------------------------------------------------------------------------------
--                                                     FORMAT FEATURE STRING --
-------------------------------------------------------------------------------


-- | Format a @FeatureStr f@ whose basic feature set has already been
--   formatted.
formatFeatureStr ∷ FeatureStr T.Text → T.Text
formatFeatureStr = T.intercalate "," ∘ NE.toList ∘ fmap formatFeature


-- | Format a @FeatureStr f@ using a function that formats the basic
--   feature set @f@.
formatFeatureStrWithFmt ∷ (f → T.Text) → FeatureStr f → T.Text
formatFeatureStrWithFmt fmt = formatFeatureStr ∘ fmap (fmap fmt)


-------------------------------------------------------------------------------
--                                                       FORMAT LEXICAL ITEM --
-------------------------------------------------------------------------------


-- | Format a @LexItem f β@ whose basic feature set and content type
--   have already been formatted.
formatLexItem ∷ LexItem T.Text T.Text → T.Text
formatLexItem (LexItem fs β)
    = "[" ⊕ β ⊕ "] ∷ [" ⊕ formatFeatureStr fs ⊕ "]"


-- | Format a @LexItem f β@ using a function that formats the basic
--   feature set @f@ along with a function that formats the content
--   type @β@.
formatLexItemWithFmt
    ∷ (f → T.Text)
    → (β → T.Text)
    → LexItem f β
    → T.Text
formatLexItemWithFmt fFmt βFmt = formatLexItem ∘ liFmt fFmt βFmt


-- | Reach into a lexical item and format its basic feature set and
--   content type.
liFmt
    ∷ (f → T.Text)
    → (β → T.Text)
    → LexItem f β
    → LexItem T.Text T.Text
liFmt fFmt βFmt li
  = LexItem (fmap fFmt <$> lexItemFeatures li)
            (βFmt ∘ lexItemContent $ li)


-------------------------------------------------------------------------------
--                                                            FORMAT GRAMMAR --
-------------------------------------------------------------------------------


-- | Format a @Grammar f β@ whose basic feature set and content type
--   have already been formatted.
formatGrammar ∷ Grammar T.Text T.Text → T.Text
formatGrammar g = lexiconStr ⊕ "\n" ⊕ startCategoryStr
  where
    lexiconStr       = T.intercalate " " ∘ fmap (\s → s ⊕ ".")  ∘ fmap formatLexItem ∘ S.toList $ lexicon g
    startCategoryStr = "startCategory(" ⊕ startCategory g ⊕ ").\n"


-- | Format a @Grammar f β@ using a function that formats the basic
--   feature set @f@ along with a function that formats the content
--   type @β@.
formatGrammarWithFmt ∷ (f → T.Text) → (β → T.Text) → Grammar f β → T.Text
formatGrammarWithFmt fFmt βFmt g = formatGrammar g'
  where
    g' = Grammar{ startCategory = fFmt ∘ startCategory $ g
                , lexicon       = S.map (liFmt fFmt βFmt) ∘ lexicon $ g
                }
