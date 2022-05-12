{-# OPTIONS_HADDOCK not-home #-}
-------------------------------------------------------------------------------
-- |
-- Module      : FormalLanguage.MG
-- Description : Common tools for working with Minimalist Grammars
-- Copyright   : © 2022, Patrick M. Niedzielski
-- License     : BSD-3-Clause
--
-- Maintainer  : patrick@pniedzielski.net
-- Stability   : alpha
-- Portability : portable
--
-- Minimalist Grammars (“MGs”; Stabler 1997, Stabler & Keenan 2003, etc) are a
-- class of closely related grammatical formalisms that incorporate ideas from
-- Chomsky’s Minimalist Program of linguistic theory, such as covert and overt
-- phrasal movement, head movement, remnant movement, and others, while still
-- being simple enough to prove important computational properties about
-- theories stated in these terms.
--
-- This module exports the most common tools for working with Minimalist
-- Grammars.  Additional functionality, such as parsers, grammatical morphisms,
-- and Minimalist Grammar algebras can be imported from other modules included
-- in this package.

module FormalLanguage.MG
  ( module FormalLanguage.MG.Feature
    -- * Bibliography
    --
    -- $bibliography
  ) where

import FormalLanguage.MG.Feature

-- $bibliography
--
--   * Stabler, E. (1997).  Derivational minimalism.  In Proceedings of the
--     First International Conference on Logical Aspects of Computational
--     Linguistics (LACL 1996), pp. 68–95.
--
--   * Stabler, E. and E. Keenan (2003).  Structural similarity within and
--     among languages.  Theoretical Computer Science 293(2), pp. 345–363.
