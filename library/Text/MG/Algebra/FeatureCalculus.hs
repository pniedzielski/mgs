{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Text.MG.Algebra.FeatureCalculus
  ( featureCalculus
  ) where

import Text.MG.Derivation ( Derivation, DerivationF(..) )
import Text.MG.Expr
import Text.MG.Feature
import Text.MG.Grammar

import Prelude.Unicode
import Control.Monad (unless)
import Data.Comp
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))


featureCalculus ∷ (Eq f, Ord f) ⇒ Derivation f β → Maybe (Expr f ())
featureCalculus = cataM featureCalculusAlgM

mergeMovers
    ∷ (Eq f, Ord f)
    ⇒ Map f (Chain f α)
    → Map f (Chain f α)
    → Maybe (Map f (Chain f α))
mergeMovers mvrs1 mvrs2
  | Map.null $ Map.intersection mvrs1 mvrs2
    = return $ Map.union mvrs1 mvrs2
  | otherwise
    = fail "Movers contain overlapping features"

addMover
    ∷ (Eq f, Ord f)
    ⇒ Chain f α
    → Map f (Chain f α)
    → Maybe (Map f (Chain f α))
addMover mvr@(Chain fs _) mvrs
  | Licensee f :| _ ← fs
  , Map.null $ Map.intersection (Map.singleton f mvr) mvrs
    = return $ Map.union (Map.singleton f mvr) mvrs
  | otherwise
    = fail "Movers contain overlapping features"

addMoverAndMerge
    ∷ (Eq f, Ord f)
    ⇒ Chain f α
    → Map f (Chain f α)
    → Map f (Chain f α)
    → Maybe (Map f (Chain f α))
addMoverAndMerge mvr@(Chain fs _) mvrs1 mvrs2
  | Licensee f :| _ ← fs
  , Map.null ∘ Map.intersection (Map.singleton f mvr)
             $ Map.intersection mvrs1 mvrs2
    = return $ Map.unions [mvrs1, mvrs2, Map.singleton f mvr]
  | otherwise
    = fail "Movers contain overlapping features"

class FeatureCalculus deriv f where
    featureCalculusAlgM ∷ AlgM Maybe deriv (Expr f ())

instance (Eq f, Ord f) ⇒ FeatureCalculus (DerivationF f β) f where
    featureCalculusAlgM (Select (LexItem fs _))
        = return $ Expr SimplexExpr (Chain fs ()) Map.empty

    featureCalculusAlgM (Merge1 (Expr SimplexExpr (Chain (Selectional f1 :| fs) ()) _)
                                (Expr _           (Chain (Categorial  f2 :| []) ()) mvrs)) = do
        unless (f1 ≡ f2) $ fail "Wrong features"
        fs' ← NE.nonEmpty fs
        return $ Expr ComplexExpr (Chain fs' ()) mvrs
    featureCalculusAlgM (Merge1 _ _) = fail "Not a merge1"

    featureCalculusAlgM (Merge2 (Expr ComplexExpr (Chain (Selectional f1 :| fs) ()) mvrs1)
                                (Expr _           (Chain (Categorial  f2 :| []) ()) mvrs2)) = do
        unless (f1 ≡ f2) $ fail "Wrong features"
        fs'   ← NE.nonEmpty fs
        mvrs' ← mergeMovers mvrs1 mvrs2
        return $ Expr ComplexExpr (Chain fs' ()) mvrs'
    featureCalculusAlgM (Merge2 _ _) = fail "Not a merge2"

    featureCalculusAlgM (Merge3 (Expr _ (Chain (Selectional f1 :| fs1) ()) mvrs1)
                                (Expr _ (Chain (Categorial  f2 :| fs2) ()) mvrs2)) = do
        unless (f1 ≡ f2) $ fail "Wrong features"
        fs1'  ← NE.nonEmpty fs1
        fs2'  ← NE.nonEmpty fs2
        mvrs' ← addMoverAndMerge (Chain fs2' ()) mvrs1 mvrs2
        return $ Expr ComplexExpr (Chain fs1' ()) mvrs'
    featureCalculusAlgM (Merge3 _ _) = fail "Not a merge3"

    featureCalculusAlgM (Move1 (Expr ComplexExpr (Chain (Licenser f1 :| fs) ()) mvrs)) = do
        fs' ← NE.nonEmpty fs
        (Chain (Licensee f2 :| []) ()) ← Map.lookup f1 mvrs
        unless (f1 ≡ f2) $ fail "No compatible trace"
        let mvrs' = Map.delete f1 mvrs
        return $ Expr ComplexExpr (Chain fs' ()) mvrs'
    featureCalculusAlgM (Move1 _) = fail "Not a move1"

    featureCalculusAlgM (Move2 (Expr ComplexExpr (Chain (Licenser f1 :| fs1) ()) mvrs)) = do
        fs1' ← NE.nonEmpty fs1
        (Chain (Licensee f2 :| fs2) ()) ← Map.lookup f1 mvrs
        fs2' ← NE.nonEmpty fs2
        unless (f1 ≡ f2) $ fail "No compatible trace"
        let mvrs' = Map.delete f1 mvrs
        mvrs'' ← addMover (Chain fs2' ()) mvrs'
        return $ Expr ComplexExpr (Chain fs1' ()) mvrs''
    featureCalculusAlgM (Move2 _) = fail "Not a move2"
