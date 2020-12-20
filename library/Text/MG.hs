{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Text.MG
  ( Feature(..)
  , FeatureStr
  , LexItem(..)
  , Derivation(..)
  , Chain(..)
  , Expr(..)
  , Grammar(..)
  , recognize
  , parse
  , doneItems
  , fillChart
  , showChart
  , derivations
  , deriv2Tree
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Multimap (SetMultimap)
import qualified Data.Multimap as Mmap
import Control.Monad.ST
import Data.STRef
import Control.Monad.Loops (iterateUntil)
import Data.Maybe (fromJust, mapMaybe, maybeToList, isNothing)
import qualified Data.List as List
import Data.Tree (Tree(..))

-- Features are given structure in an MG.
data Feature f
    = Selectional f
    | Categorial f
    | Licenser f
    | Licensee f
  deriving (Eq, Ord, Show, Read, Functor)

type FeatureStr f = NonEmpty (Feature f)

-- Lexical item over syntactic features `Feature f` and nonsyntactic
-- features α.
data LexItem f β = LexItem (FeatureStr f) β
  deriving (Eq, Ord, Show, Read, Functor)

isEmptyLexItem :: LexItem f String -> Bool
isEmptyLexItem (LexItem _ β) = null β

lexItemFeatures :: LexItem f β -> FeatureStr f
lexItemFeatures (LexItem fs _) = fs

lexItemContent :: LexItem f β -> β
lexItemContent (LexItem _ β) = β

-- Derivation
data Derivation f β
    = Select (LexItem f β)
    | Merge1 (Derivation f β) (Derivation f β)
    | Merge2 (Derivation f β) (Derivation f β)
    | Merge3 (Derivation f β) (Derivation f β)
    | Move1 (Derivation f β)
    | Move2 (Derivation f β)
  deriving (Eq, Show, Read, Functor)


data Chain f β = Chain (FeatureStr f) β
  deriving (Eq, Ord, Show, Read, Functor)

-- Lexical?, main chain, movers
data Expr f β = Expr Bool (Chain f β) (Map f (Chain f β))
  deriving (Eq, Ord, Show, Read, Functor)

-- MGs are lexicalized formalisms
data Grammar f β = Grammar
    { startCategory :: f
    , lexicon :: Set (LexItem f β)
    }
  deriving (Eq, Ord, Show, Read)

emptyItems :: Grammar f String -> [LexItem f String]
emptyItems = Set.toList . Set.filter isEmptyLexItem . lexicon

valueItems :: Eq β =>  Grammar f β -> β -> [LexItem f β]
valueItems g β = (Set.toList . Set.filter (\x -> lexItemContent x == β) . lexicon) g


-------------------------------------------------------------------------------


type Pos = (Int, Int)

data ItemChain f = ItemChain Pos (FeatureStr f)
  deriving (Eq, Ord, Show, Read)

data Item f = Item Bool (ItemChain f) (Map f (ItemChain f))
  deriving (Eq, Ord, Show, Read)


newtype Agenda f = Agenda [Item f]
  deriving (Eq, Ord, Show, Read)

isEmptyAgenda :: Agenda f -> Bool
isEmptyAgenda (Agenda []) = True
isEmptyAgenda _           = False

nextAgendaItem :: Agenda f -> Maybe (Item f, [Item f])
nextAgendaItem (Agenda [])    = Nothing
nextAgendaItem (Agenda (x:xs)) = Just (x,xs)

data Chart f = Chart
    { leftEdges  :: SetMultimap Int (Item f)
    , rightEdges :: SetMultimap Int (Item f)
    , movers     :: SetMultimap f   (Item f)
    , allItems   :: Set (Item f)
    }
  deriving (Eq, Ord, Show, Read)

emptyChart :: Chart f
emptyChart = Chart
  { leftEdges  = Mmap.empty
  , rightEdges = Mmap.empty
  , movers     = Mmap.empty
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
    parseFilter (Item _ (ItemChain _ (Categorial f' :| [])) mvrs) = f == f' && Map.null mvrs
    parseFilter _ = False

addChartItem :: Ord f => Chart f -> Item f -> Chart f
addChartItem c i@(Item _ (ItemChain (l,r) _) mvrs)
    = Chart { leftEdges  = Mmap.prepend l i $ leftEdges  c
            , rightEdges = Mmap.prepend r i $ rightEdges c
            , movers     = movers'
            , allItems   = Set.insert i $ allItems c
            }
  where
    movers' = foldr (`Mmap.prepend` i) (movers c) (Map.keys mvrs)

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
    empties = [ (Item True
                 (ItemChain (i,i) (lexItemFeatures li))
                 Map.empty,
                 OpAxiom li)
              | i  <- [0..length s]
              , li <- emptyItems g
              ]
    paired = zip s [0..]
    lexItems = paired >>= \(x,i) -> map (\li -> (Item True (ItemChain (i,i+1) (lexItemFeatures li)) Map.empty, OpAxiom li)) $ valueItems g x


initialAgenda :: Grammar f String -> [String] -> Agenda f
initialAgenda g s = Agenda (empties ++ lexItems)
  where
    empties = [ Item True
                (ItemChain (i,i) (lexItemFeatures li))
                Map.empty
              | i  <- [0..length s]
              , li <- emptyItems g
              ]
    paired = zip s [0..]
    lexItems = paired >>= \(x,i) -> map (\li -> Item True (ItemChain (i,i+1) (lexItemFeatures li)) Map.empty) $ valueItems g x


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

merge1All :: (Eq f, Ord f) =>  Chart f -> Item f -> [(Item f, DerivOp f)]
merge1All c i@(Item _ (ItemChain (p,q) _) _) =
  mapMaybe (`merge1` i) $ (Set.toList $ q `Mmap.find` leftEdges c) ++ (Set.toList $ p `Mmap.find` rightEdges c)

merge1 :: Eq f => Item f -> Item f -> Maybe (Item f, DerivOp f)
merge1 i1@(Item True (ItemChain (p,q1) (Selectional f1 :| fs)) _)
       i2@(Item _    (ItemChain (q2,v) (Categorial  f2 :| [])) mvrs)
  | f1 == f2
  , q1 == q2
    = Just (Item False (ItemChain (p,v) (NonEmpty.fromList fs)) mvrs, OpMerge1 i1 i2)
merge1 i2@(Item _    (ItemChain (q2,v) (Categorial  f2 :| [])) mvrs)
       i1@(Item True (ItemChain (p,q1) (Selectional f1 :| fs)) _)
  | f1 == f2
  , q1 == q2
    = Just (Item False (ItemChain (p,v) (NonEmpty.fromList fs)) mvrs, OpMerge1 i1 i2)
merge1 _ _ = Nothing

merge2All :: (Eq f, Ord f) =>  Chart f -> Item f -> [(Item f, DerivOp f)]
merge2All c i@(Item _ (ItemChain (p,q) _) _) =
  mapMaybe (`merge2` i) $ (Set.toList $ q `Mmap.find` leftEdges c) ++ (Set.toList $ p `Mmap.find` rightEdges c)

merge2 :: (Eq f, Ord f) => Item f -> Item f -> Maybe (Item f, DerivOp f)
merge2 i1@(Item False (ItemChain (p1,q) (Selectional f1 :| fs)) mvrs1)
       i2@(Item _     (ItemChain (v,p2) (Categorial  f2 :| [])) mvrs2)
  | f1 == f2
  , p1 == p2
  , Map.null $ Map.intersection mvrs1 mvrs2
    = Just (Item False (ItemChain (v,q) (NonEmpty.fromList fs)) (Map.union mvrs1 mvrs2), OpMerge2 i1 i2)
merge2 i2@(Item _     (ItemChain (v,p2) (Categorial  f2 :| [])) mvrs2)
       i1@(Item False (ItemChain (p1,q) (Selectional f1 :| fs)) mvrs1)
  | f1 == f2
  , p1 == p2
  , Map.null $ Map.intersection mvrs1 mvrs2
    = Just (Item False (ItemChain (v,q) (NonEmpty.fromList fs)) (Map.union mvrs1 mvrs2), OpMerge2 i1 i2)
merge2 _ _ = Nothing

merge3All :: (Eq f, Ord f) => Chart f -> Item f -> [(Item f, DerivOp f)]
merge3All c i =
  mapMaybe (`merge3` i) $ (Set.toList $ allItems c)

merge3 :: (Eq f, Ord f) => Item f -> Item f -> Maybe (Item f, DerivOp f)
merge3 i1@(Item _ (ItemChain (p,q) (Selectional f1 :| fs)) mvrs1)
       i2@(Item _ (ItemChain (v,w) (Categorial  f2 :| (Licensee f:fs'))) mvrs2)
  | f1 == f2
  , Map.null $ Map.intersection (Map.singleton f (ItemChain (v,w) (Licensee f :| fs'))) $ Map.intersection mvrs1 mvrs2
    = Just (Item False (ItemChain (p,q) (NonEmpty.fromList fs)) (Map.unions [mvrs1,mvrs2, Map.singleton f (ItemChain (v,w) (Licensee f :| fs'))]), OpMerge3 i1 i2)
merge3 i2@(Item _ (ItemChain (v,w) (Categorial  f2 :| (Licensee f:fs'))) mvrs2)
       i1@(Item _ (ItemChain (p,q) (Selectional f1 :| fs)) mvrs1)
  | f1 == f2
  , Map.null $ Map.intersection (Map.singleton f (ItemChain (v,w) (Licensee f :| fs'))) $ Map.intersection mvrs1 mvrs2
    = Just (Item False (ItemChain (p,q) (NonEmpty.fromList fs)) (Map.unions [mvrs1,mvrs2, Map.singleton f (ItemChain (v,w) (Licensee f :| fs'))]), OpMerge3 i1 i2)
merge3 _ _ = Nothing

move1 :: (Eq f, Ord f) => Item f -> [(Item f, DerivOp f)]
move1 i@(Item False (ItemChain (p1,q) (Licenser f :| fs)) mvrs)
    = [ (Item False (ItemChain (v,q) (NonEmpty.fromList fs)) mvrs', OpMove1 i)
      | ItemChain (v,p2) fs' <- mvr
      , p1 == p2
      , length fs' == 1
      ]
  where
    mvr   = maybeToList $ f `Map.lookup` mvrs
    mvrs' = f `Map.delete` mvrs
move1 _ = []

move2 :: (Eq f, Ord f) => Item f -> [(Item f, DerivOp f)]
move2 i@(Item False (ItemChain (p,q) (Licenser f :| fs)) mvrs)
    = [ (Item False (ItemChain (p,q) (NonEmpty.fromList fs)) $
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
showItem (Item True mainChain ms) =
  "[" ++ showItemChain mainChain ++ (Map.elems ms >>= showItemChain) ++ "]ₛ\n"
showItem (Item False mainChain ms) =
  "[" ++ showItemChain mainChain ++ (Map.elems ms >>= showItemChain) ++ "]ₜ\n"

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
derivationItem df i = (Set.toList $ Mmap.find i df) >>= derivationOp df

derivationOp :: Ord f => DerivForest f -> DerivOp f -> [Derivation f String]
derivationOp _  (OpAxiom  li)    = [Select li]
derivationOp df (OpMerge1 i1 i2) = [ Merge1 d1 d2
                                   | d1 <- derivationItem df i1
                                   , d2 <- derivationItem df i2
                                   ]
derivationOp df (OpMerge2 i1 i2) = [ Merge2 d1 d2
                                   | d1 <- derivationItem df i1
                                   , d2 <- derivationItem df i2
                                   ]
derivationOp df (OpMerge3 i1 i2) = [ Merge3 d1 d2
                                   | d1 <- derivationItem df i1
                                   , d2 <- derivationItem df i2
                                   ]
derivationOp df (OpMove1 i1)     = [ Move1 d1 | d1 <- derivationItem df i1 ]
derivationOp df (OpMove2 i1)     = [ Move2 d1 | d1 <- derivationItem df i1 ]


-------------------------------------------------------------------------------


deriv2Tree :: Derivation f String -> Tree String
deriv2Tree (Select li) = Node { rootLabel = if null $ lexItemContent li then "ε" else lexItemContent li
                              , subForest = []
                              }
deriv2Tree (Merge1 d1 d2) = Node { rootLabel = "Merge1"
                                 , subForest = [deriv2Tree d1, deriv2Tree d2]
                                 }
deriv2Tree (Merge2 d1 d2) = Node { rootLabel = "Merge2"
                                 , subForest = [deriv2Tree d1, deriv2Tree d2]
                                 }
deriv2Tree (Merge3 d1 d2) = Node { rootLabel = "Merge3"
                                 , subForest = [deriv2Tree d1, deriv2Tree d2]
                                 }
deriv2Tree (Move1 d) = Node { rootLabel = "Move1"
                            , subForest = [deriv2Tree d]
                            }
deriv2Tree (Move2 d) = Node { rootLabel = "Move2"
                            , subForest = [deriv2Tree d]
                            }
