{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_HADDOCK show-extensions #-}
-------------------------------------------------------------------------------
-- |
-- Module      : FormalLanguage.MG.Feature
-- Description : Feature calculus for Minimalist Grammars
-- Copyright   : © 2022, Patrick M. Niedzielski
-- License     : BSD-3-Clause
--
-- Maintainer  : patrick@pniedzielski.net
-- Stability   : alpha
-- Portability : portable
--
-- Following Chomsky (1995), all structure-building operations in MGs are
-- driven by the matching and checking of /features/, purely formal objects
-- that are specified by the grammar.  Unlike early Minimalism, though, the
-- features in MGs specify /which/ structure-building operations take place:
-- certain features drive __Merge__, for instance, and certain other features
-- drive __Move__.  In variants of MGs that add additional flavors of
-- structure-building operations, such as head movement or ATB-movement, these
-- operations are usually accompanied by (and driven by) additional flavors of
-- features.
--
-- This module models the feature calculus of MGs.  Like the rest of the
-- package, we follow the simplified formulation of MGs from Stabler (1998),
-- Stabler & Keenan (2003), and others, which doesn’t include extensions like
-- covert movement, head movement, or directional selection.  This formulation
-- is weakly equivalent to MGs with the above extensions, and is used
-- frequently in the literature as a simple testbed on which to work, so we
-- will stick with this formulation here.
--
-- __Nota bene__!  The API of this module is experimental, and /will change/
-- quite a bit in order to support MGs with more operations.

module FormalLanguage.MG.Feature
  ( -- * Role-annotated Features
    -- $roleannotated
    Feature(..)
  , basicFeature

    -- ** Polarity
    -- $polarity
  , Polarity(..)
  , polarity
  , pos
  , neg

    -- ** Operation
    -- $operation
  , Operation(..)
  , operation
  , opMerge
  , opMove

    -- ** Matching
    -- $matches
  , matches
  , matchingFeature

    -- * Example Usage
    -- $example

    -- * References
    -- $references
  ) where

import Prelude.Unicode

-------------------------------------------------------------------------------
-- $roleannotated
--
-- Each Minimalist Grammar specifies a finite set of so-called /basic
-- features/.  For a grammar of fragment of English, for instance, we may
-- choose the following basic features:
--
-- \[
--   F = \{\, N, D, V, v, T, C, kase, wh \,\}
-- \]
--
-- From these basic features are built a set of /role-annotated features/,
-- which indicate the structure-building operation they perform (__Merge__ or
-- __Move__), along with one of two opposite polarities (positive or negative);
-- two opposite role-annotated features for the same operation built from the
-- same basic feature come together and are deleted to drive that operation.
--
-- In the simplified formulation of Minimalist Grammars, the set of
-- role-annotated features \( \mathrm{Feat}(F) \) built from a set of basic
-- features \( F \) is constructed as the union of the following four disjoint
-- sets:
--
--   * __Categorial__ features, \( \{\, f \;|\; f \in F \,\} \), which have
--     /negative/ polarity and drive the operation __Merge__;
--
--   * __Selectional__ features, \( \{\, =f \;|\; f \in F \,\} \), which have
--     /positive/ polarity and drive the operation __Merge__;
--
--   * __Licensee__ features, \( \{\, -f \;|\; f \in F \,\} \), which have
--     /negative/ polarity and drive the operation __Move__; and
--
--   * __Licenser__ features, \( \{\, +f \;|\; f \in F \,\} \), which have
--     /positive/ polarity and drive the operation __Move__.

-- | A role-annotated feature built from the set of basic features @f@.
data Feature f
  = Categorial  f  -- ^ Categorial feature built of basic feature @f@
  | Selectional f  -- ^ Selectional feature built of basic feature @f@
  | Licensee    f  -- ^ Licensee feature built of basic feature @f@
  | Licenser    f  -- ^ Licenser feature built of basic feature @f@
  deriving (Eq, Show, Read, Functor)

-- | Order role-annotated features first by their basic feature, then their
--   role.
instance Ord f ⇒ Ord (Feature f) where
  compare f1 f2
    | basicFeatureComparison ≡ EQ
    = case (f1, f2) of
        (Categorial  _, Categorial  _) → EQ
        (Categorial  _, Selectional _) → LT
        (Categorial  _, Licensee    _) → LT
        (Categorial  _, Licenser    _) → LT
        (Selectional _, Categorial  _) → GT
        (Selectional _, Selectional _) → EQ
        (Selectional _, Licensee    _) → LT
        (Selectional _, Licenser    _) → LT
        (Licensee    _, Categorial  _) → GT
        (Licensee    _, Selectional _) → GT
        (Licensee    _, Licensee    _) → EQ
        (Licensee    _, Licenser    _) → LT
        (Licenser    _, Categorial  _) → GT
        (Licenser    _, Selectional _) → GT
        (Licenser    _, Licensee    _) → GT
        (Licenser    _, Licenser    _) → EQ
    | otherwise = basicFeatureComparison
    where
      basicFeatureComparison =
        basicFeature f1 `compare` basicFeature f2

-- | If the basic features are bounded, so are role-annotated features.
instance Bounded f ⇒ Bounded (Feature f) where
  minBound = Categorial minBound
  maxBound = Licenser   maxBound

-- | If the basic features have a total order, so do role-annotated features.
instance Enum f ⇒ Enum (Feature f) where
  toEnum x | x `mod` 4 ≡ 0 = Categorial  f
           | x `mod` 4 ≡ 1 = Selectional f
           | x `mod` 4 ≡ 2 = Licensee    f
           | x `mod` 4 ≡ 3 = Licenser    f
           | otherwise     = error "x `mod` 4 is not 0, 1, 2, or 3"
    where f = toEnum (x `div` 4)

  fromEnum (Categorial  f) = 0 + 4 ⋅ fromEnum f
  fromEnum (Selectional f) = 1 + 4 ⋅ fromEnum f
  fromEnum (Licensee    f) = 2 + 4 ⋅ fromEnum f
  fromEnum (Licenser    f) = 3 + 4 ⋅ fromEnum f

-- | The basic feature on which a role-annotated feature is built.
basicFeature ∷ Feature f → f
basicFeature (Categorial  f) = f
basicFeature (Selectional f) = f
basicFeature (Licensee    f) = f
basicFeature (Licenser    f) = f

-------------------------------------------------------------------------------
-- $polarity
--
-- Each role-annotated feature has a __polarity__ associated with it.  Features
-- of opposite polarities are required to induce a structure-building operation.
--
-- Conventionally, the two polarities are called /positive/ and /negative/
-- polarities; unfortunately, these are not descriptive names and don’t provide
-- a good intuition for how they function in grammars.  Roughly, a /negative/
-- feature is a property of some subderivation, specifying how it is licensed
-- in a larger derivation: 'Categorial' features, for instance, describe which
-- phrase can be selected for in a __Merge__, and 'Licensee' features describe
-- which phrases can be searched for by a __Move__.  Conversely, a /positive/
-- feature is a property of a head that licenses subderivations: 'Selectional'
-- features license the __Merge__ of a subderivation with a particular
-- 'Categorial' feature, and 'Licenser' features license the __Move__ of a
-- subderivaiton with a particular 'Licensee' feature.

-- | The polarity of a role-annotated feature.
data Polarity
  = Pos  -- ^ Positive featural polarity
  | Neg  -- ^ Negative featural polarity
  deriving (Eq, Ord, Show, Read)

-- | Return the polarity of a role-annotated feature.
polarity ∷ Feature f → Polarity
polarity (Categorial  _) = Neg
polarity (Selectional _) = Pos
polarity (Licensee    _) = Neg
polarity (Licenser    _) = Pos

-- | Return whether a role-annotated feature has positive polarity.
pos ∷ Feature f → Bool
pos = (≡ Pos) ∘ polarity

-- | Return whether a role-annotated feature has negative polarity.
neg ∷ Feature f → Bool
neg = (≡ Neg) ∘ polarity

-------------------------------------------------------------------------------
-- $operation
--
-- Simplified Minimalist Grammars have only two structure-building operations:
-- __Merge__ and __Move__.  Each of these are driven by two features of
-- opposing polarities; which must match in their basic feature and operation.

-- | The operation driven by a role-annotated feature.
data Operation
  = OpMerge  -- ^ Merge driven by some feature
  | OpMove   -- ^ Move driven by some feature
  deriving (Eq, Ord, Show, Read)

-- | Return the operation that a role-annotated feature drives.
operation ∷ Feature f → Operation
operation (Categorial  _) = OpMerge
operation (Selectional _) = OpMerge
operation (Licensee    _) = OpMove
operation (Licenser    _) = OpMove

-- | Return whether a role-annotated feature drives __Merge__.
opMerge ∷ Feature f → Bool
opMerge = (≡ OpMerge) ∘ operation

-- | Return whether a role-annotated feature drives __Move__.
opMove ∷ Feature f → Bool
opMove = (≡ OpMove) ∘ operation

-------------------------------------------------------------------------------
-- $matches
--
-- In order to drive some structure-building operation, two features need to be
-- present in the derivation, which share a basic feature and operation, but
-- differ in their polarity.  These two features then delete, and drive the
-- particular operation they are both specified for.
--
-- For instance, given two partial derivations @d1@ and @d2@ with
-- role-annotated features @=f@ and @f@, respectively, we can form a new
-- derivation @d3 = Merge(d1,d2)@.

-- | Return whether two role-annotated features cancel each other out to drive
--   an operation.
matches ∷ Eq f ⇒ Feature f → Feature f → Bool
matches (Categorial  f1) (Selectional f2) = f1 ≡ f2
matches (Selectional f1) (Categorial  f2) = f1 ≡ f2
matches (Licensee    f1) (Licenser    f2) = f1 ≡ f2
matches (Licenser    f1) (Licensee    f2) = f1 ≡ f2
matches _                _                = False

-- | Return the role-annotated feature that would cancel a given feature out.
matchingFeature ∷ Feature f → Feature f
matchingFeature (Categorial  f) = Selectional f
matchingFeature (Selectional f) = Categorial  f
matchingFeature (Licensee    f) = Licenser    f
matchingFeature (Licenser    f) = Licensee    f

-------------------------------------------------------------------------------
-- $example
--
-- The most straightforward usage of this module is to use a finite set as the
-- basic feature set, such as 'Char':
--
-- > type MyFeature = FormalLanguage.MG.Feature Char
--
-- Any type where we use finitely many values and which is an instance of 'Eq'
-- and 'Ord' should work well with this package.
--
-- Role-annotated features are purely formal objects, so your features can be
-- internally complex.  For instance, if you want to model concord phenomena,
-- you might extend your basic feature set to track φ-features:
--
-- > data PhiFeats    = PhiFeats PersonFeats NumFeats
-- > data NumFeats    = Sg | Pl
-- > data PersonFeats = Fst | Snd | Thd
-- >
-- > -- Track the PhiFeats along the clausal spine.
-- > data Feats = TP PhiFeats | DP PhiFeats | VP PhiFeats | …
-- >            | CaseLicense | Focus | …
-- >
-- > -- Role-annotated features from this basic feature set.
-- > type MyFeature = FormalLanguage.MG.Feature.Feature Feats
-- >
-- > -- Wants a VP with a 2pl DP inside to agree with.
-- > let myFeat = FormalLanguage.MG.Feature.Selectional ∘ VP $ PhiFeats Snd Pl
--
-- Your MG lexicon would have to properly manage the @PhiFeat@s being
-- propagated along the clausal spine, of course.
--
-- This should enable us to safely and automatically generate simple MGs from
-- more complex MG-formalisms, which often require adding new features.

-------------------------------------------------------------------------------
-- $references
--
--   * Chomsky, N. (1995).  /The Minimalist Program/.  Cambridge, MA: The MIT
--     Press.
--
--   * Stabler, E. (1998).  Acquiring Languages with Movement.  Syntax 1(1),
--     pp. 72–97.
--
--   * Stabler, E. and E. Keenan (2003).  Structural similarity within and
--     among languages.  Theoretical Computer Science 293(2), pp. 345–363.
