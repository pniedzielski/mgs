{-# LANGUAGE DeriveFunctor          #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Text.MG.Grammar
Description : A Minimalist Grammar
Copyright   : © 2021, Patrick M. Niedzielski
License     : LGPL-3.0-or-later
Maintainer  : patrick@pniedzielski.net
Stability   : experimental
Portability : non-portable

Early proposals within the Minimalism Program (Chomsky 1993, 1995)
advanced the idea that all syntactic variation is lexical knowledge,
/viz./ that syntactic parameters are stored as properties (/features/)
of particular items in the lexicon.  The mechanisms of
structure-building are then assumed to be universal and fixed; their
application within a syntactic derivation is driven by the presence or
absence of particular features on the lexical items that have entered
the derivation.  Baker (2008) calls this idea the /Borer-Chomsky
Conjecture/ (BCC).  For Chomsky, this reduces learning syntactic
variation to learning the lexicon of a language, a task that is
independently required of the learner.

Stabler’s (1997, /et seq/) Derivational Minimalism attempts to
formalize Chomsky’s early Minimalism.  Derivational Minimalism, like
Chomsky’s proposal, is a highly lexicalized grammatical formalism, in
which all differences between grammars are held within a /lexicon/: a
set of terminals that are annotated with a string of syntactic
features which drive the derivations.  Features are checked off in
sequence by structure-building operations.  Unlike in early
Minimalism, though, the features in Derivational Minimalism specify
/which/ structure-building operations take place: certain features
drive __Merge__, for instance, while certain other features drive
__Move__.  Within this framework, we factor all nondeterminism in the
derivation out into lexical selection; once lexical items are
selected, the derivation proceeds deterministically.  This obviates
problems like whether to prefer __Merge__ to __Move__ during the
derivation, at the expense of requiring any such generalizations to be
stated as constraints on possible lexicons.

Since originally presented, there has been a cottage industry of
adding operations to (and removing operations from) Stabler’s original
formalization, with the intent of bringing the formalization closer to
current syntactic theorizing, as well as with the intent of better
understanding the formal properties of the system.  This represents an
important line of work, but it is difficult to get hands-on experience
with these related formalisms, as existing parsers and implementations
tend to implement only one particular set of operations.

One of the goals of this package is to allow extensible parsers with
different features and operations available.  Grammars with different
operations are represented as different Haskell types, which allows
morphisms between grammar types to be represented safely: converting a
grammar with head-movement to a weakly-equivalent one using only
remnant movement becomes a function from one 'Grammar' to another
'Grammar'.  This design allows modular construction and interpretation
of different variants of Minimalist Grammars.
-}

module Text.MG.Grammar
    ( -- * Grammar

      -- $grammar
      Grammar(..)
    , emptyItems
    , nonEmptyItems

      -- * Lexical Items

      -- $lexitem
    , LexItem(..)
    , isEmpty
    ) where

import           Prelude.Unicode
import           Data.List.NonEmpty( NonEmpty )
import           Data.Set( Set )
import qualified Data.Set as Set


-------------------------------------------------------------------------------
--                                                                   GRAMMAR --
-------------------------------------------------------------------------------


{-$grammar

We define a Minimalist Grammar over a carrier monoid @α@ as a
five-tuple ⟨@α@, @f@, /Ops/, @c@, /Lex/⟩, where @f@ is the set of
/basic features/, /Ops/ is the set of structure-building operations,
@c@∈@f@ is the category of completed expressions of the grammar, and
/Lex/ is a set of lexical items built of feature strings and values
from @α@.
-}

-- | A Minimalist Grammar built from a finite set of basic features
--   @f@ and a carrier set @α@, with @c@ as @startCategory@, and /Lex/
--   as @lexicon@.
data Grammar f α = Grammar
    { -- | The category of completed expressions
      startCategory ∷ f
      -- | The lexical items within the grammar
    , lexicon       ∷ Set (LexItem f α)
    }
  deriving (Eq, Ord, Show, Read)

-- | All lexical items of a grammar @g@ that have 'mempty' values.
emptyItems ∷ (Monoid α, Eq α) ⇒ Grammar f α → Set (LexItem f α)
emptyItems = Set.filter isEmpty ∘ lexicon

-- | All lexical items of a grammar @g@ that have non-'mempty' values.
nonEmptyItems ∷ (Monoid α, Eq α) ⇒ Grammar f α → Set (LexItem f α)
nonEmptyItems = Set.filter (not ∘ isEmpty) ∘ lexicon


-------------------------------------------------------------------------------
--                                                             LEXICAL ITEMS --
-------------------------------------------------------------------------------


{-$lexitem

A lexical item in a Minimalist Grammar pairs a sequence of syntactic
features with a value.
-}

-- | A lexical item in a Minimalist Grammar with basic features @f@
--   and carrier monoid @α@.
data LexItem f α = LexItem (NonEmpty f) α
  deriving (Eq, Ord, Show, Read, Functor)

-- | Whether a given lexical item has 'mempty' as its value.
isEmpty ∷ (Monoid α, Eq α) ⇒ LexItem f α → Bool
isEmpty (LexItem _ α) = α ≡ mempty
