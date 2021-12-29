{-# LANGUAGE OverloadedStrings #-}

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
import Text.MG.Derivation
import Text.MG.Expr

import Data.Multimap (SetMultimap)
import qualified Data.Multimap as Mmap
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Control.Monad.ST
import Data.STRef
import Control.Monad.Loops (iterateUntil)
import Data.Maybe (fromJust, mapMaybe, maybeToList, isNothing)
import qualified Data.List as List
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Text as T

import Prelude.Unicode
import Data.Monoid.Unicode

import Data.Comp

tshow :: Show α ⇒ α → T.Text
tshow = T.pack ∘ show


-------------------------------------------------------------------------------


type Pos = (Int, Int)

data ItemChain f = ItemChain Pos (FeatureStr f)
  deriving (Eq, Ord, Show, Read)

data Item f = Item ExprType (ItemChain f) (Map f (ItemChain f))
  deriving (Eq, Ord, Show, Read)


-------------------------------------------------------------------------------


type Agenda f = [Item f]

isEmptyAgenda :: Agenda f -> Bool
isEmptyAgenda = List.null

nextAgendaItem :: Agenda f -> Maybe (Item f, [Item f])
nextAgendaItem = List.uncons

initialAgenda :: Grammar f T.Text -> [T.Text] -> Agenda f
initialAgenda g s = empties ++ lexItems
  where
    empties = [ Item SimplexExpr
                (ItemChain (i,i) (lexItemFeatures li))
                Map.empty
              | i  <- [0..length s]
              , li <- emptyItems g
              ]
    lexItems = [ Item SimplexExpr
                 (ItemChain (i,i+1) (lexItemFeatures li))
                 Map.empty
               | i <- [0..length s-1]
               , li <- valueItems g $ s !! i
               ]


-------------------------------------------------------------------------------


data Chart f = Chart
    { leftEdges  :: Vector (Set (Item f))
    , rightEdges :: Vector (Set (Item f))
    , allItems   :: Set (Item f)
    }
  deriving (Eq, Ord, Show, Read)

initialChart :: [T.Text] -> ST s (STRef s (Chart f))
initialChart s = newSTRef $ Chart
  { leftEdges  = Vector.replicate (length s + 1) Set.empty
  , rightEdges = Vector.replicate (length s + 1) Set.empty
  , allItems   = Set.empty
  }

isChartDone :: (Eq f, Ord f) => Chart f -> f -> Int -> Bool
isChartDone c f n = not . null $ doneItems c f n

doneItems :: (Eq f, Ord f) => Chart f -> f -> Int -> [Item f]
doneItems c f n = Set.toList parses
  where
    ls = leftEdges  c Vector.! 0
    rs = rightEdges c Vector.! n
    full = Set.intersection ls rs
    parses = Set.filter parseFilter full
    parseFilter (Item _ (ItemChain _ (Categorial f' :| [])) mvrs) = f == f' && Map.null mvrs
    parseFilter _ = False

addChartItem :: Ord f => Item f -> Chart f -> Chart f
addChartItem i@(Item _ (ItemChain (l,r) _) _) c
    = Chart { leftEdges  = le Vector.// [(l, Set.insert i $ le Vector.! l)]
            , rightEdges = re Vector.// [(r, Set.insert i $ re Vector.! r)]
            , allItems   = Set.insert i ai
            }
  where
    le = leftEdges  c
    re = rightEdges c
    ai = allItems   c


-------------------------------------------------------------------------------


type DerivOp f = DerivationF f T.Text (Item f)

type DerivForest f = SetMultimap (Item f) (DerivOp f)


initialForest :: Ord f => Grammar f T.Text -> [T.Text] -> DerivForest f
initialForest g s = Mmap.fromList (empties ++ lexItems)
  where
    empties = [ (Item SimplexExpr
                 (ItemChain (i,i) (lexItemFeatures li))
                 Map.empty,
                 Select li)
              | i  <- [0..length s]
              , li <- emptyItems g
              ]
    paired = zip s [0..]
    lexItems = paired >>= \(x,i) -> map (\li -> (Item SimplexExpr (ItemChain (i,i+1) (lexItemFeatures li)) Map.empty, Select li)) $ valueItems g x


-------------------------------------------------------------------------------


recognize :: (Eq f, Ord f) => Grammar f T.Text -> [T.Text] -> Bool
recognize g s =
    isChartDone chart (startCategory g) (length s)
  where
    (chart, _) = fillChart g s

parse :: (Eq f, Ord f) => Grammar f T.Text -> [T.Text] -> DerivForest f
parse g s = snd $ fillChart g s

fillChart :: (Eq f, Ord f) => Grammar f T.Text -> [T.Text] -> (Chart f, DerivForest f)
fillChart g s =
    runST (do
        chart  <- initialChart s
        forest <- newSTRef $ initialForest g s
        agenda <- newSTRef $ initialAgenda g s
        _ <- iterateUntil isEmptyAgenda
                 (do
                     agenda' <- readSTRef agenda
                     chart'  <- readSTRef chart
                     forest' <- readSTRef forest
                     let (item, rest) = fromJust $ nextAgendaItem agenda'
                     let proven = merge1All chart' item
                               ++ merge2All chart' item
                               ++ merge3All chart' item
                               ++ move1 item
                               ++ move2 item
                     let newItems = List.filter (\x -> not . Set.member x $ allItems chart') . List.delete item . map fst $ proven
                     writeSTRef agenda $ rest `List.union` newItems
                     writeSTRef chart  $ addChartItem item chart'
                     writeSTRef forest $ foldr (uncurry Mmap.prepend) forest' proven
                     readSTRef agenda)
        chart'  <- readSTRef chart
        forest' <- readSTRef forest
        return (chart', forest'))

itemsAtLeftEdge :: Ord f => Int -> Chart f -> [Item f]
itemsAtLeftEdge n c = Set.toList $ leftEdges c Vector.! n

itemsAtRightEdge :: Ord f => Int -> Chart f -> [Item f]
itemsAtRightEdge n c = Set.toList $ rightEdges c Vector.! n

itemsAnywhere :: Chart f -> [Item f]
itemsAnywhere = Set.toList . allItems

type UnaryOp  f = Item f -> [(Item f, DerivOp f)]
type BinaryOp f = Item f -> Item f -> Maybe (Item f, DerivOp f)
type BinaryContext f = Chart f -> Item f -> [(Item f, DerivOp f)]

flipNegFirst :: Eq f => BinaryOp f -> BinaryOp f
flipNegFirst op i1@(Item _ (ItemChain _ (f :| _)) _) i2
    | pos f     = op i1 i2
    | otherwise = op i2 i1

merge1All :: (Eq f, Ord f) => BinaryContext f
merge1All c i@(Item _ (ItemChain (p,q) _) _) =
  mapMaybe (flipNegFirst merge1 i) $ itemsAtLeftEdge q c ++ itemsAtRightEdge p c

merge1 :: Eq f => BinaryOp f
merge1 i1@(Item SimplexExpr (ItemChain (p,q1) (Selectional f1 :| fs)) _)
       i2@(Item _           (ItemChain (q2,v) (Categorial  f2 :| [])) mvrs)
  | f1 == f2
  , q1 == q2
    = Just (Item ComplexExpr (ItemChain (p,v) (NonEmpty.fromList fs)) mvrs, Merge1 i1 i2)
merge1 _ _ = Nothing

merge2All :: (Eq f, Ord f) => BinaryContext f
merge2All c i@(Item _ (ItemChain (p,q) _) _) =
  mapMaybe (flipNegFirst merge2 i) $ itemsAtLeftEdge q c ++ itemsAtRightEdge p c

merge2 :: (Eq f, Ord f) => BinaryOp f
merge2 i1@(Item ComplexExpr (ItemChain (p1,q) (Selectional f1 :| fs)) mvrs1)
       i2@(Item _           (ItemChain (v,p2) (Categorial  f2 :| [])) mvrs2)
  | f1 == f2
  , p1 == p2
  , Map.null $ Map.intersection mvrs1 mvrs2
    = Just (Item ComplexExpr (ItemChain (v,q) (NonEmpty.fromList fs)) (Map.union mvrs1 mvrs2), Merge2 i1 i2)
merge2 _ _ = Nothing

merge3All :: (Eq f, Ord f) => BinaryContext f
merge3All c i =
  mapMaybe (flipNegFirst merge3 i) $ itemsAnywhere c

merge3 :: (Eq f, Ord f) => BinaryOp f
merge3 i1@(Item _ (ItemChain (p,q) (Selectional f1 :| fs)) mvrs1)
       i2@(Item _ (ItemChain (v,w) (Categorial  f2 :| (Licensee f:fs'))) mvrs2)
  | f1 == f2
  , w <= p || q <= v
  , Map.null $ Map.intersection (Map.singleton f (ItemChain (v,w) (Licensee f :| fs'))) $ Map.intersection mvrs1 mvrs2
    = Just (Item ComplexExpr (ItemChain (p,q) (NonEmpty.fromList fs)) (Map.unions [mvrs1,mvrs2, Map.singleton f (ItemChain (v,w) (Licensee f :| fs'))]), Merge3 i1 i2)
merge3 _ _ = Nothing

move1 :: (Eq f, Ord f) => UnaryOp f
move1 i@(Item ComplexExpr (ItemChain (p1,q) (Licenser f :| fs)) mvrs)
    = [ (Item ComplexExpr (ItemChain (v,q) (NonEmpty.fromList fs)) mvrs', Move1 i)
      | ItemChain (v,p2) fs' <- mvr
      , p1 == p2
      , length fs' == 1
      ]
  where
    mvr   = maybeToList $ f `Map.lookup` mvrs
    mvrs' = f `Map.delete` mvrs
move1 _ = []

move2 :: (Eq f, Ord f) => UnaryOp f
move2 i@(Item ComplexExpr (ItemChain (p,q) (Licenser f :| fs)) mvrs)
    = [ (Item ComplexExpr (ItemChain (p,q) (NonEmpty.fromList fs)) $
        Map.insert (feat fs') (ItemChain (v,w) (NonEmpty.fromList fs')) mvrs', Move2 i)
      | ItemChain (v,w) (_ :| fs') <- mvr
      , not $ null fs'
      , isNothing $ feat fs' `Map.lookup` mvrs'
      ]
  where
    mvr   = maybeToList $ f `Map.lookup` mvrs
    feat (Licensee f':_) = f'
    feat _ = undefined
    mvrs' = f `Map.delete` mvrs
move2 _ = []


showChart :: Chart T.Text -> T.Text
showChart = foldMap showItem ∘ Set.toList ∘ allItems

showItem :: Item T.Text -> T.Text
showItem (Item SimplexExpr mainChain ms) =
  "[" ⊕ showItemChain mainChain ⊕ foldMap showItemChain ms ⊕ "]ₛ\n"
showItem (Item ComplexExpr mainChain ms) =
  "[" ⊕ showItemChain mainChain ⊕ foldMap showItemChain ms ⊕ "]ₜ\n"

showItemChain :: ItemChain T.Text -> T.Text
showItemChain (ItemChain (p,q) fs) =
  "(" ⊕ tshow p ⊕ "," ⊕ tshow q ⊕ ")" ⊕ showFeatureStr fs

showFeatureStr :: FeatureStr T.Text -> T.Text
showFeatureStr = foldMap showFeature

showFeature :: Feature T.Text -> T.Text
showFeature (Categorial f)  = " "  ⊕ f
showFeature (Selectional f) = " =" ⊕ f
showFeature (Licensee f)    = " -" ⊕ f
showFeature (Licenser f)    = " +" ⊕ f


-------------------------------------------------------------------------------


-- | Compute the 'Derivation's of an 'Item', according to a derivation
-- forest.
derivations
    :: Ord f
    => DerivForest f
    -> Item f
    -> [Derivation f T.Text]
derivations = anaM . derivationCoalgM

-- | A monadic coalgebra to generate a list of 'Derivation's from a
-- single 'Item', according to a derivation forest.
derivationCoalgM
    :: Ord f
    => DerivForest f
    -> CoalgM [] (DerivationF f T.Text) (Item f)
derivationCoalgM df item = Set.toList $ Mmap.find item df
