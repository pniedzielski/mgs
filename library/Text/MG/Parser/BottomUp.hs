{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}

module Text.MG.Parser.BottomUp
  ( recognize
  , parse
  , doneItems
  , fillChart
  , showChart
  , derivations
  ) where

import Text.MG.Feature
import Text.MG.Grammar
    ( valueItems, emptyItems, lexItemFeatures, Grammar(startCategory) )
import Text.MG.Derivation
import Text.MG.Expr

import Data.Multimap (SetMultimap)
import qualified Data.Multimap as Mmap
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Control.Monad.ST
import Data.STRef
import Control.Monad.Loops (iterateUntil)
import qualified Data.IxSet as Ix
import Data.IxSet ( (@=), (|||) )
import Data.Maybe (fromJust, mapMaybe, maybeToList, isNothing)
import qualified Data.List as List
import qualified Data.Text as T
import           Data.Typeable (Typeable)

import Prelude.Unicode
import Control.Monad.Unicode
import Data.Monoid.Unicode

import Data.Comp

tshow ∷ Show α ⇒ α → T.Text
tshow = T.pack ∘ show


-------------------------------------------------------------------------------


type Pos = (Int, Int)

type ItemChain f = Chain f Pos
type Item      f = Expr f Pos


-------------------------------------------------------------------------------


type Agenda f = [Item f]

isEmptyAgenda ∷ Agenda f → Bool
isEmptyAgenda = List.null

nextAgendaItem ∷ Agenda f → Maybe (Item f, [Item f])
nextAgendaItem = List.uncons

initialAgenda ∷ Grammar f T.Text → [T.Text] → Agenda f
initialAgenda g s =
    empties ⧺ lexItems
  where
    empties = [ Expr SimplexExpr
                (Chain (lexItemFeatures li) (i,i))
                Map.empty
              | i  ← [0..length s]
              , li ← emptyItems g
              ]
    lexItems = [ Expr SimplexExpr
                 (Chain (lexItemFeatures li) (i,i+1))
                 Map.empty
               | i  ← [0..length s-1]
               , li ← valueItems g $ s !! i
               ]


-------------------------------------------------------------------------------


newtype MainChainLeftIdx    = MainChainLeftIdx Int deriving (Eq, Ord)
newtype MainChainRightIdx   = MainChainRightIdx Int deriving (Eq, Ord)
newtype MainChainHeadFeat f = MainChainHeadFeat (Feature f) deriving (Eq, Ord)
newtype MainChainTailFeats  = MainChainTailFeats Bool deriving (Eq, Ord)
newtype MoverLeftIdx f      = MoverLeftIdx (Feature f, Int) deriving (Eq, Ord)
newtype MoverRightIdx f     = MoverRightIdx (Feature f, Int) deriving (Eq, Ord)
newtype MoverHeadFeat f     = MoverHeadFeat (Feature f) deriving (Eq, Ord)
newtype MoverTailFeats f    = MoverTailFeats (Feature f, Bool) deriving (Eq, Ord)
newtype MoversEmpty         = MoversEmpty Bool deriving (Eq, Ord)

getMainChainLeftIdx ∷ Item f → [MainChainLeftIdx]
getMainChainLeftIdx (Expr _ (Chain _ (l,_)) _) = [MainChainLeftIdx l]

getMainChainRightIdx ∷ Item f → [MainChainRightIdx]
getMainChainRightIdx (Expr _ (Chain _ (_,r)) _) = [MainChainRightIdx r]

getExprType ∷ Item f → [ExprType]
getExprType (Expr t _ _) = [t]

getMainChainHeadFeat ∷ Item f → [MainChainHeadFeat f]
getMainChainHeadFeat (Expr _ (Chain (f :| _) _) _) = [MainChainHeadFeat f]

getMainChainTailFeats ∷ Item f → [MainChainTailFeats]
getMainChainTailFeats (Expr _ (Chain (_ :| fs) _) _) = [MainChainTailFeats ∘ not ∘ null $ fs]

getMoverLeftIdx ∷ Item f → [MoverLeftIdx f]
getMoverLeftIdx (Expr _ _ mvrs) = assocToIdx <$> Map.assocs mvrs
  where
    assocToIdx (k, Chain _ (l,_)) = MoverLeftIdx (Licenser k, l)

getMoverRightIdx ∷ Item f → [MoverRightIdx f]
getMoverRightIdx (Expr _ _ mvrs) = assocToIdx <$> Map.assocs mvrs
  where
    assocToIdx (k, Chain _ (_,r)) = MoverRightIdx (Licenser k, r)

getMoverHeadFeat ∷ Item f → [MoverHeadFeat f]
getMoverHeadFeat (Expr _ _ mvrs) = MoverHeadFeat ∘ Licenser <$> Map.keys mvrs

getMoverTailFeats ∷ Item f → [MoverTailFeats f]
getMoverTailFeats (Expr _ _ mvrs) = assocToTail <$> Map.assocs mvrs
  where
    assocToTail (k, Chain (_ :| fs) _) = MoverTailFeats (Licenser k, not ∘ null $ fs)

getMoversEmpty ∷ Item f → [MoversEmpty]
getMoversEmpty (Expr _ _ mvrs) = [MoversEmpty ∘ Map.null $ mvrs]

instance (Eq f, Ord f, Typeable f) ⇒ Ix.Indexable (Item f) where
    empty = Ix.ixSet
        [ Ix.ixFun getMainChainLeftIdx
        , Ix.ixFun getMainChainRightIdx
        , Ix.ixFun getExprType
        , Ix.ixFun getMainChainHeadFeat
        , Ix.ixFun getMainChainTailFeats
        , Ix.ixFun getMoverLeftIdx
        , Ix.ixFun getMoverRightIdx
        , Ix.ixFun getMoverHeadFeat
        , Ix.ixFun getMoverTailFeats
        , Ix.ixFun getMoversEmpty
        ]


-------------------------------------------------------------------------------


type Chart f = Ix.IxSet (Item f)

initialChart ∷ (Eq f, Ord f, Typeable f) ⇒ ST s (STRef s (Chart f))
initialChart = newSTRef Ix.empty

isChartDone ∷ (Eq f, Ord f, Typeable f) ⇒ Chart f → f → Int → Bool
isChartDone c f n = not ∘ null $ doneItems c f n

doneItems ∷ (Eq f, Ord f, Typeable f) ⇒ Chart f → f → Int → [Item f]
doneItems c f n = Ix.toList $
    c @= MainChainLeftIdx  0
      @= MainChainRightIdx n
      @= MainChainHeadFeat (Categorial f)
      @= MoversEmpty True

addChartItem ∷ (Eq f, Ord f, Typeable f) ⇒ Item f → Chart f → Chart f
addChartItem = Ix.insert


-------------------------------------------------------------------------------


type DerivOp     f = DerivationF f T.Text (Item f)
type DerivForest f = SetMultimap (Item f) (DerivOp f)


initialForest
    ∷ Ord f
    ⇒ Grammar f T.Text
    → [T.Text]
    → DerivForest f
initialForest g s =
    Mmap.fromList (empties ⧺ lexItems)
  where
    empties  = [ (Expr SimplexExpr
                  (Chain (lexItemFeatures li) (i,i))
                  Map.empty,
                  Select li)
               | i  ← [0..length s]
               , li ← emptyItems g
               ]
    paired   = zip s [0..]
    lexItems = paired ≫= \(x,i) →
        map (simpleChain i) $ valueItems g x
    simpleChain i li =
        ( Expr SimplexExpr
              (Chain (lexItemFeatures li) (i,i+1))
              Map.empty
        , Select li
        )


-------------------------------------------------------------------------------


recognize
    ∷ (Eq f, Ord f, Typeable f)
    ⇒ Grammar f T.Text
    → [T.Text]
    → Bool
recognize g s =
    isChartDone chart (startCategory g) (length s)
  where
    chart = fst $ fillChart g s

parse
    ∷ (Eq f, Ord f, Typeable f)
    ⇒ Grammar f T.Text
    → [T.Text]
    → DerivForest f
parse g s = snd $ fillChart g s

fillChart
    ∷ (Eq f, Ord f, Typeable f)
    ⇒ Grammar f T.Text
    → [T.Text]
    → (Chart f, DerivForest f)
fillChart g s = runST $ do
    chart  ← initialChart
    forest ← newSTRef $ initialForest g s
    agenda ← newSTRef $ initialAgenda g s
    _ ← iterateUntil isEmptyAgenda $ do
        agenda' ← readSTRef agenda
        chart'  ← readSTRef chart
        forest' ← readSTRef forest
        let (item, rest) = fromJust $ nextAgendaItem agenda'
        let proven = merge1All chart' item
                   ⧺ merge2All chart' item
                   ⧺ merge3All chart' item
                   ⧺ move1 item
                   ⧺ move2 item
        let isNewItem = \x → not ∘ Set.member x $ Ix.toSet chart'
        let newItems  = List.filter isNewItem
                      ∘ List.delete item
                      ∘ map fst
                      $ proven
        writeSTRef agenda $ rest `List.union` newItems
        writeSTRef chart  $ addChartItem item chart'
        writeSTRef forest $ foldr (uncurry Mmap.prepend) forest' proven
        readSTRef agenda
    chart'  ← readSTRef chart
    forest' ← readSTRef forest
    return (chart', forest')

type UnaryOp
    = ∀f. (Eq f, Ord f)
    ⇒ Item f
    → [(Item f, DerivOp f)]
type BinaryOp
    = ∀f. (Eq f, Ord f)
    ⇒ Item f
    → Item f
    → Maybe (Item f, DerivOp f)
type BinaryContext
    = ∀f. (Eq f, Ord f, Typeable f)
    ⇒ Chart f
    → Item f
    → [(Item f, DerivOp f)]

flipNegFirst ∷ BinaryOp → BinaryOp
flipNegFirst op i1@(Expr _ (Chain (f :| _) _) _) i2
    | pos f     = op i1 i2
    | otherwise = op i2 i1

merge1All ∷ BinaryContext
merge1All c i@(Expr _ (Chain _ (p,q)) _) =
    mapMaybe (flipNegFirst merge1 i) items
  where
    items = Ix.toList $ c @= MainChainLeftIdx q ||| c @= MainChainRightIdx p

merge1 ∷ BinaryOp
merge1 i1@(Expr SimplexExpr (Chain (Selectional f1 :| fs) (p,q1)) _)
       i2@(Expr _           (Chain (Categorial  f2 :| []) (q2,v)) mvrs)
  | f1 == f2
  , q1 == q2
    = Just (Expr ComplexExpr (Chain (NonEmpty.fromList fs) (p,v)) mvrs, Merge1 i1 i2)
merge1 _ _ = Nothing

merge2All ∷ BinaryContext
merge2All c i@(Expr _ (Chain _ (p,q)) _) =
    mapMaybe (flipNegFirst merge2 i) items
  where
    items = Ix.toList $ c @= MainChainLeftIdx q ||| c @= MainChainRightIdx p

merge2 ∷ BinaryOp
merge2 i1@(Expr ComplexExpr (Chain (Selectional f1 :| fs) (p1,q)) mvrs1)
       i2@(Expr _           (Chain (Categorial  f2 :| []) (v,p2)) mvrs2)
  | f1 == f2
  , p1 == p2
  , Map.null $ Map.intersection mvrs1 mvrs2
    = Just (Expr ComplexExpr (Chain (NonEmpty.fromList fs) (v,q)) (Map.union mvrs1 mvrs2), Merge2 i1 i2)
merge2 _ _ = Nothing

merge3All ∷ BinaryContext
merge3All c i =
    mapMaybe (flipNegFirst merge3 i) items
  where
    items = Ix.toList c

merge3 ∷ BinaryOp
merge3 i1@(Expr _ (Chain (Selectional f1 :| fs) (p,q)) mvrs1)
       i2@(Expr _ (Chain (Categorial  f2 :| (Licensee f:fs')) (v,w)) mvrs2)
  | f1 == f2
  , w <= p || q <= v
  , Map.null $ Map.intersection (Map.singleton f (Chain (Licensee f :| fs') (v,w))) $ Map.intersection mvrs1 mvrs2
    = Just (Expr ComplexExpr (Chain (NonEmpty.fromList fs) (p,q)) (Map.unions [mvrs1,mvrs2, Map.singleton f (Chain (Licensee f :| fs') (v,w))]), Merge3 i1 i2)
merge3 _ _ = Nothing

move1 ∷ UnaryOp
move1 i@(Expr ComplexExpr (Chain (Licenser f :| fs) (p1,q)) mvrs)
    = [ (Expr ComplexExpr (Chain (NonEmpty.fromList fs) (v,q)) mvrs', Move1 i)
      | Chain fs' (v,p2) ← mvr
      , p1 == p2
      , length fs' == 1
      ]
  where
    mvr   = maybeToList $ f `Map.lookup` mvrs
    mvrs' = f `Map.delete` mvrs
move1 _ = []

move2 ∷ UnaryOp
move2 i@(Expr ComplexExpr (Chain (Licenser f :| fs) (p,q)) mvrs)
    = [ (Expr ComplexExpr (Chain (NonEmpty.fromList fs) (p,q)) $
        Map.insert (feat fs') (Chain (NonEmpty.fromList fs') (v,w)) mvrs', Move2 i)
      | Chain (_ :| fs') (v,w) ← mvr
      , not $ null fs'
      , isNothing $ feat fs' `Map.lookup` mvrs'
      ]
  where
    mvr   = maybeToList $ f `Map.lookup` mvrs
    feat (Licensee f':_) = f'
    feat _ = undefined
    mvrs' = f `Map.delete` mvrs
move2 _ = []


showChart ∷ Chart T.Text → T.Text
showChart = foldMap showItem ∘ Ix.toList

showItem ∷ Item T.Text → T.Text
showItem (Expr SimplexExpr mainChain ms) =
  "[" ⊕ showItemChain mainChain ⊕ foldMap showItemChain ms ⊕ "]ₛ\n"
showItem (Expr ComplexExpr mainChain ms) =
  "[" ⊕ showItemChain mainChain ⊕ foldMap showItemChain ms ⊕ "]ₜ\n"

showItemChain ∷ ItemChain T.Text → T.Text
showItemChain (Chain fs (p,q)) =
  "(" ⊕ tshow p ⊕ "," ⊕ tshow q ⊕ ")" ⊕ showFeatureStr fs

showFeatureStr ∷ FeatureStr T.Text → T.Text
showFeatureStr = foldMap showFeature

showFeature ∷ Feature T.Text → T.Text
showFeature (Categorial f)  = " "  ⊕ f
showFeature (Selectional f) = " =" ⊕ f
showFeature (Licensee f)    = " -" ⊕ f
showFeature (Licenser f)    = " +" ⊕ f


-------------------------------------------------------------------------------


-- | Compute the 'Derivation's of an 'Item', according to a derivation
-- forest.
derivations
    ∷ Ord f
    ⇒ DerivForest f
    → Item f
    → [Derivation f T.Text]
derivations = anaM ∘ derivationCoalgM

-- | A monadic coalgebra to generate a list of 'Derivation's from a
-- single 'Item', according to a derivation forest.
derivationCoalgM
    ∷ Ord f
    ⇒ DerivForest f
    → CoalgM [] (DerivationF f T.Text) (Item f)
derivationCoalgM df item =
    Set.toList $ Mmap.find item df
