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

import Data.Comp


-------------------------------------------------------------------------------


type Pos = (Int, Int)

data ItemMainChain f = ItemMainChain Pos (FeatureStr f)
  deriving (Eq, Ord, Show, Read)

data ItemChain f = ItemChain Pos (FeatureStr f)
  deriving (Eq, Ord, Show, Read)

data Item f = Item ExprType (ItemMainChain f) (Map f (ItemChain f))
  deriving (Eq, Ord, Show, Read)


-------------------------------------------------------------------------------


newtype Agenda f = Agenda [Item f]
  deriving (Eq, Ord, Show, Read)

isEmptyAgenda :: Agenda f -> Bool
isEmptyAgenda (Agenda []) = True
isEmptyAgenda _           = False

nextAgendaItem :: Agenda f -> Maybe (Item f, [Item f])
nextAgendaItem (Agenda [])    = Nothing
nextAgendaItem (Agenda (x:xs)) = Just (x,xs)

initialAgenda :: Grammar f String -> [String] -> Agenda f
initialAgenda g s = Agenda (empties ++ lexItems)
  where
    empties = [ Item SimplexExpr
                (ItemMainChain (i,i) (lexItemFeatures li))
                Map.empty
              | i  <- [0..length s]
              , li <- emptyItems g
              ]
    paired = zip s [0..]
    lexItems = paired >>= \(x,i) -> map (\li -> Item SimplexExpr (ItemMainChain (i,i+1) (lexItemFeatures li)) Map.empty) $ valueItems g x


-------------------------------------------------------------------------------


data Chart f = Chart
    { leftEdges  :: SetMultimap Int (Item f)
    , rightEdges :: SetMultimap Int (Item f)
    , allItems   :: Set (Item f)
    }
  deriving (Eq, Ord, Show, Read)

emptyChart :: Chart f
emptyChart = Chart
  { leftEdges  = Mmap.empty
  , rightEdges = Mmap.empty
  , allItems   = Set.empty
  }

isChartDone :: (Eq f, Ord f) => Chart f -> f -> Int -> Bool
isChartDone c f n = not . null $ doneItems c f n

doneItems :: (Eq f, Ord f) => Chart f -> f -> Int -> [Item f]
doneItems c f n = Set.toList parses
  where
    ls = 0 `Mmap.find` leftEdges  c
    rs = n `Mmap.find` rightEdges c
    full = Set.intersection ls rs
    parses = Set.filter parseFilter full
    parseFilter (Item _ (ItemMainChain _ (Categorial f' :| [])) mvrs) = f == f' && Map.null mvrs
    parseFilter _ = False

addChartItem :: Ord f => Chart f -> Item f -> Chart f
addChartItem c i@(Item _ (ItemMainChain (l,r) _) _)
    = Chart { leftEdges  = Mmap.prepend l i $ leftEdges  c
            , rightEdges = Mmap.prepend r i $ rightEdges c
            , allItems   = Set.insert i $ allItems c
            }


-------------------------------------------------------------------------------


data DerivOp f
    = OpAxiom  (LexItem f String)
    | OpMerge1 (Item f) (Item f)
    | OpMerge2 (Item f) (Item f)
    | OpMerge3 (Item f) (Item f)
    | OpMove1  (Item f)
    | OpMove2  (Item f)
  deriving (Eq, Ord, Show, Read)

type DerivForest f = SetMultimap (Item f) (DerivOp f)


initialForest :: Ord f => Grammar f String -> [String] -> DerivForest f
initialForest g s = Mmap.fromList (empties ++ lexItems)
  where
    empties = [ (Item SimplexExpr
                 (ItemMainChain (i,i) (lexItemFeatures li))
                 Map.empty,
                 OpAxiom li)
              | i  <- [0..length s]
              , li <- emptyItems g
              ]
    paired = zip s [0..]
    lexItems = paired >>= \(x,i) -> map (\li -> (Item SimplexExpr (ItemMainChain (i,i+1) (lexItemFeatures li)) Map.empty, OpAxiom li)) $ valueItems g x


-------------------------------------------------------------------------------


recognize :: (Eq f, Ord f) => Grammar f String -> [String] -> Bool
recognize g s =
    isChartDone chart (startCategory g) (length s)
  where
    (chart, _) = fillChart g s

parse :: (Eq f, Ord f) => Grammar f String -> [String] -> DerivForest f
parse g s = snd $ fillChart g s

fillChart :: (Eq f, Ord f) => Grammar f String -> [String] -> (Chart f, DerivForest f)
fillChart g s =
    runST (do
        chart  <- newSTRef emptyChart
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
                     writeSTRef agenda $ Agenda (rest `List.union` newItems)
                     writeSTRef chart  $ addChartItem chart' item
                     writeSTRef forest $ foldr (uncurry Mmap.prepend) forest' proven
                     readSTRef agenda)
        chart'  <- readSTRef chart
        forest' <- readSTRef forest
        return (chart', forest'))

itemsAtLeftEdge :: Ord f => Int -> Chart f -> [Item f]
itemsAtLeftEdge n = Set.toList . Mmap.find n . leftEdges

itemsAtRightEdge :: Ord f => Int -> Chart f -> [Item f]
itemsAtRightEdge n = Set.toList . Mmap.find n . rightEdges

itemsAnywhere :: Chart f -> [Item f]
itemsAnywhere = Set.toList . allItems

type UnaryOp  f = Item f -> [(Item f, DerivOp f)]
type BinaryOp f = Item f -> Item f -> Maybe (Item f, DerivOp f)
type BinaryContext f = Chart f -> Item f -> [(Item f, DerivOp f)]

flipNegFirst :: Eq f => BinaryOp f -> BinaryOp f
flipNegFirst op i1@(Item _ (ItemMainChain _ (f :| _)) _) i2 | pos f     = op i1 i2
                                                            | otherwise = op i2 i1

merge1All :: (Eq f, Ord f) => BinaryContext f
merge1All c i@(Item _ (ItemMainChain (p,q) _) _) =
  mapMaybe (flipNegFirst merge1 i) $ itemsAtLeftEdge q c ++ itemsAtRightEdge p c

merge1 :: Eq f => BinaryOp f
merge1 i1@(Item SimplexExpr (ItemMainChain (p,q1) (Selectional f1 :| fs)) _)
       i2@(Item _           (ItemMainChain (q2,v) (Categorial  f2 :| [])) mvrs)
  | f1 == f2
  , q1 == q2
    = Just (Item ComplexExpr (ItemMainChain (p,v) (NonEmpty.fromList fs)) mvrs, OpMerge1 i1 i2)
merge1 _ _ = Nothing

merge2All :: (Eq f, Ord f) => BinaryContext f
merge2All c i@(Item _ (ItemMainChain (p,q) _) _) =
  mapMaybe (flipNegFirst merge2 i) $ itemsAtLeftEdge q c ++ itemsAtRightEdge p c

merge2 :: (Eq f, Ord f) => BinaryOp f
merge2 i1@(Item ComplexExpr (ItemMainChain (p1,q) (Selectional f1 :| fs)) mvrs1)
       i2@(Item _           (ItemMainChain (v,p2) (Categorial  f2 :| [])) mvrs2)
  | f1 == f2
  , p1 == p2
  , Map.null $ Map.intersection mvrs1 mvrs2
    = Just (Item ComplexExpr (ItemMainChain (v,q) (NonEmpty.fromList fs)) (Map.union mvrs1 mvrs2), OpMerge2 i1 i2)
merge2 _ _ = Nothing

merge3All :: (Eq f, Ord f) => BinaryContext f
merge3All c i =
  mapMaybe (flipNegFirst merge3 i) $ itemsAnywhere c

merge3 :: (Eq f, Ord f) => BinaryOp f
merge3 i1@(Item _ (ItemMainChain (p,q) (Selectional f1 :| fs)) mvrs1)
       i2@(Item _ (ItemMainChain (v,w) (Categorial  f2 :| (Licensee f:fs'))) mvrs2)
  | f1 == f2
  , w <= p || q <= v
  , Map.null $ Map.intersection (Map.singleton f (ItemChain (v,w) (Licensee f :| fs'))) $ Map.intersection mvrs1 mvrs2
    = Just (Item ComplexExpr (ItemMainChain (p,q) (NonEmpty.fromList fs)) (Map.unions [mvrs1,mvrs2, Map.singleton f (ItemChain (v,w) (Licensee f :| fs'))]), OpMerge3 i1 i2)
merge3 _ _ = Nothing

move1 :: (Eq f, Ord f) => UnaryOp f
move1 i@(Item ComplexExpr (ItemMainChain (p1,q) (Licenser f :| fs)) mvrs)
    = [ (Item ComplexExpr (ItemMainChain (v,q) (NonEmpty.fromList fs)) mvrs', OpMove1 i)
      | ItemChain (v,p2) fs' <- mvr
      , p1 == p2
      , length fs' == 1
      ]
  where
    mvr   = maybeToList $ f `Map.lookup` mvrs
    mvrs' = f `Map.delete` mvrs
move1 _ = []

move2 :: (Eq f, Ord f) => UnaryOp f
move2 i@(Item ComplexExpr (ItemMainChain (p,q) (Licenser f :| fs)) mvrs)
    = [ (Item ComplexExpr (ItemMainChain (p,q) (NonEmpty.fromList fs)) $
        Map.insert (feat fs') (ItemChain (v,w) (NonEmpty.fromList fs')) mvrs', OpMove2 i)
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


showChart :: Chart Char -> String
showChart c =
  Set.toList (allItems c) >>= showItem

showItem :: Item Char -> String
showItem (Item SimplexExpr mainChain ms) =
  "[" ++ showItemMainChain mainChain ++ (Map.elems ms >>= showItemChain) ++ "]ₛ\n"
showItem (Item ComplexExpr mainChain ms) =
  "[" ++ showItemMainChain mainChain ++ (Map.elems ms >>= showItemChain) ++ "]ₜ\n"

showItemMainChain :: ItemMainChain Char -> String
showItemMainChain (ItemMainChain (p,q) fs) =
  "(" ++ show p ++ "," ++ show q ++ ")" ++ showFeatureStr fs

showItemChain :: ItemChain Char -> String
showItemChain (ItemChain (p,q) fs) =
  "(" ++ show p ++ "," ++ show q ++ ")" ++ showFeatureStr fs

showFeatureStr :: FeatureStr Char -> String
showFeatureStr (f :| fs) = showFeature f ++ (fs >>= showFeature)

showFeature :: Feature Char -> String
showFeature (Categorial f)  = [' ', f]
showFeature (Selectional f) = [' ', '=', f]
showFeature (Licensee f)    = [' ', '-', f]
showFeature (Licenser f)    = [' ', '+', f]


-------------------------------------------------------------------------------


derivations :: Ord f => DerivForest f -> [Item f] -> [Derivation f String]
derivations df is = is >>= derivationItem df

derivationItem :: Ord f => DerivForest f -> Item f -> [Derivation f String]
derivationItem df i = Set.toList (Mmap.find i df) >>= derivationOp df

derivationOp :: Ord f => DerivForest f -> DerivOp f -> [Derivation f String]
derivationOp _  (OpAxiom  li)    = [ Term $ Select li]
derivationOp df (OpMerge1 i1 i2) = [ Term $ Merge1 d1 d2
                                   | d1 <- derivationItem df i1
                                   , d2 <- derivationItem df i2
                                   ]
derivationOp df (OpMerge2 i1 i2) = [ Term $ Merge2 d1 d2
                                   | d1 <- derivationItem df i1
                                   , d2 <- derivationItem df i2
                                   ]
derivationOp df (OpMerge3 i1 i2) = [ Term $ Merge3 d1 d2
                                   | d1 <- derivationItem df i1
                                   , d2 <- derivationItem df i2
                                   ]
derivationOp df (OpMove1 i1)     = [ Term $ Move1 d1 | d1 <- derivationItem df i1 ]
derivationOp df (OpMove2 i1)     = [ Term $ Move2 d1 | d1 <- derivationItem df i1 ]
