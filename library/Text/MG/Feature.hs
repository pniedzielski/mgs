{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Text.MG.Feature
Description : Feature calculus for a Minimalist Grammar

Copyright   : © 2021, Patrick M. Niedzielski
License     : LGPL-3.0-or-later
Maintainer  : patrick@pniedzielski.net
Stability   : experimental
Portability : non-portable

Following Chomsky (1995), all structure-building operations in MGs are
driven by the matching and checking of /features/, purely formal
objects that are specified by the grammar. Unlike early Minimalism,
though, the features in MGs specify /which/ structure-building
operations take place: certain features drive __Merge__, for instance,
and certain other features drive __Move__.

This module models the feature calculus of MGs. Like the rest of the
package, we follow the simplified formulation of MGs from Stabler
(1999), Stabler & Keenan (2003), and others, which doesn’t include
extensions like covert movement, head movement, or directional
selection. This formulation is weakly equivalent to MGs with the above
extensions, and is used frequently in the literature as a simple
testbed, so we will stick with this formulation here.

__Nota bene__! The API of this module is experimental, and
/will change/ quite a bit in order to support MGs with more
operations.

== Example Usage

The most straightforward usage of this module is to use a finite set
as the basic feature set, such as 'Char':

> type MyFeature = Text.MG.Feature Char

Any type where we use finitely many values and that is an instance of
'Eq' and 'Ord' should work well with this package.

Role-annotated features are purely formal objects, so your features
can be internally complex. For instance, if you want to model concord
phenomena, you might extend your basic feature set to track
φ-features:

> data PhiFeats    = PhiFeats PersonFeats NumFeats
> data NumFeats    = Sg | Pl
> data PersonFeats = Fst | Snd | Thd
>
> -- Track the PhiFeats along the clausal spine.
> data Feats = TP PhiFeats | DP PhiFeats | VP PhiFeats | …
>            | CaseLicense | …
>
> -- Role-annotated features from this basic feature set.
> type MyFeature = Text.MG.Feature.Feature Feats
>
> -- Wants a VP with a 2pl DP inside to agree with.
> let myFeat = Text.MG.Feature.Selectional ∘ VP $ PhiFeats Snd Pl

Your MG lexicon would have to properly manage the @PhiFeat@s being
propagated along the clausal spine, of course.

This should enable us to safely and automatically generate simple MGs
from more complex MG-formalisms, which often requires adding new
features.

== References

  * Chomsky (1995): /The Minimalist Program/.
    [(doi)](https://doi.org/10.7551/mitpress/9780262527347.001.0001)
  * Stabler (1999): “Remnant movement and complexity”.
    [(pdf)](https://linguistics.ucla.edu/people/stabler/eps-fhcg98.pdf)
  * Stabler & Keenan (2003): “Structural similarity within and among
    languages”. [(doi)](https://doi.org/10.1016/S0304-3975(01\)00351-6)

-}

module Text.MG.Feature
  ( -- * Feature calculus

    -- $feature
    Feature(..)
  , basicFeature

    -- ** Operation

    -- $operation
  , mergeable
  , moveable

    -- ** Polarity

    -- $polarity
  , pos
  , neg

    -- * Feature strings

    -- $featurestrings
  , FeatureStr
  ) where

import Data.List.NonEmpty (NonEmpty)
import Prelude.Unicode


-------------------------------------------------------------------------------
--                                                          FEATURE CALCULUS --
-------------------------------------------------------------------------------


{-$feature

In simplified Minimalist Grammars, given a finite set \( F \) of
objects called /basic features/, we define a set \( \mathcal{F}(F) \)
of /role-annotated features/ , which drive the structure-building
operations in a derivation. Role-annotated features come in four
disjoint varieties:
-}

-- | A role-annotated feature built from a finite set of basic
--   features @f@.
data Feature f
    -- | For all basic features @f@, @f@. Categorial features are
    --   negative features checked by Merge.
    = Categorial f
    -- | For all basic features @f@, @=f@. Selectional features are
    --   positive features drive Merge.
    | Selectional f
    -- | For all basic features @f@, @-f@. Licensee features are
    --   negative features checked by Move.
    | Licensee f
    -- | For all basic features @f@, @+f@. Licenser features are
    --   positive features checked by Move.
    | Licenser f
  deriving (Eq, Ord, Show, Read, Functor)

-- | The basic feature on which a given role-annotated feature is
--   built.
basicFeature ∷ Feature f → f
basicFeature (Selectional f) = f
basicFeature (Categorial  f) = f
basicFeature (Licenser    f) = f
basicFeature (Licensee    f) = f
{-# INLINABLE basicFeature #-}


-------------------------------------------------------------------------------
--                                                                 OPERATION --
-------------------------------------------------------------------------------


{-$operation

Each feature triggers a specific structure-building operation. There
are only two such operations in simplified MGs, __Merge__ and
__Move__, so each feature either triggers __Merge__ or triggers
__Move__.
-}


-- | Whether a 'Feature' drives the __Merge__ operation.
--
--   Note that a role-annotated feature must either drive __Merge__ or
--   __Move__, but not both:
--
--   prop> mergeable f `xor` moveable f
mergeable ∷ Feature f → Bool
mergeable (Selectional _) = True
mergeable (Categorial _)  = True
mergeable (Licenser _)    = False
mergeable (Licensee _)    = False
{-# INLINABLE mergeable #-}

-- | Whether a 'Feature' drives the __Move__ operation.
--
--   Note that a role-annotated feature must either drive __Merge__ or
--   __Move__, but not both:
--
--   prop> moveable f `xor` mergable f
moveable ∷ Feature f → Bool
moveable (Selectional _) = False
moveable (Categorial _)  = False
moveable (Licenser _)    = True
moveable (Licensee _)    = True
{-# INLINABLE moveable #-}


-------------------------------------------------------------------------------
--                                                                  POLARITY --
-------------------------------------------------------------------------------


{-$polarity

We divide role-annotated features into two /polarities/: /positive/
and /negative/. Structure-building operations take place when two
matching features of the opposite polarity are checked and deleted.
The head with the positive feature is the one that projects. For
example, if a verb selects for and merges with a DP object to form a
verb phrase, the verb would be specified with a @=d@ (positive)
selectional feature, while the DP object would be specified with a @d@
(negative) categorial feature. Similarly, if a question-forming
complementizer moves a wh-phrase to its specifier to form a CP, the
complementizer would be specified with a @+wh@ (positive feature),
while the moving wh-phrase would be specified with a @-wh@ (negative)
categorial feature.

Polarity, along with which operation a feature is checked by,
cross-categorizes the set of role-annotated features:

+---------------------------+-------------------------------------+
|                           | /Operation/                         |
|                           +-------------------+-----------------+
|                           | __Merge__         | __Move__        |
+------------+--------------+-------------------+-----------------+
| /Polarity/ | __Positive__ | Categorial (@=f@) | Licenser (@+f@) |
|            +--------------+-------------------+-----------------+
|            | __Negative__ | Selectional (@f@) | Licensee (@-f@) |
+------------+--------------+-------------------+-----------------+

The following functions check the polarity of a 'Feature':

-}

-- | Whether a 'Feature' has positive polarity.
--
--   Note that a role-annotated feature must be either positive or
--   negative, but not both:
--
--   prop> pos f `xor` neg f
pos ∷ Feature f → Bool
pos (Selectional _) = True
pos (Categorial _)  = False
pos (Licenser _)    = True
pos (Licensee _)    = False
{-# INLINABLE pos #-}

-- | Whether a 'Feature' has negative polarity.
--
--   Note that a role-annotated feature must be either positive or
--   negative, but not both:
--
--   prop> neg f `xor` pos f
neg ∷ Feature f → Bool
neg = not ∘ pos


-------------------------------------------------------------------------------
--                                                           FEATURE STRINGS --
-------------------------------------------------------------------------------


{-$featurestrings

An MG derivation checks off features one at a time in the order they
are listed. Because every lexical item has to have at least one
feature, we provide a data structure 'FeatureStr', which allows us to
retrieve the next 'Feature' efficiently and which preserves the
invariant that at least one 'Feature' is in the sequence.

While a simplified MG allows only a finite lexicon, composed of finite
feature strings, we can generalize this while preserving the weak
generative capacity to MGs with rational relations as lexicons. In the
future, the type will change in order to accommodate this.
-}

-- | A sequence of features, from which we can retrieve the next
--   feature.
type FeatureStr f = NonEmpty (Feature f)
