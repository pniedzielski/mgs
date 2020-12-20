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
  , fillChart
  , showChart
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


data Op = OAxiom | OMerge1 | OMerge2 | OMerge3 | OMove1 | OMove2
  deriving (Eq, Ord, Show, Read)



type Pos = (Int, Int)

data ItemChain f = ItemChain Pos (FeatureStr f)
  deriving (Eq, Ord, Show, Read)

data Item f = Item Op Bool (ItemChain f) (Map f (ItemChain f))
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
isChartDone c f n = not $ Set.null parses
  where
    ls = 0 `Mmap.find` leftEdges  c
    rs = n `Mmap.find` rightEdges c
    full = Set.intersection ls rs
    parses = Set.filter parseFilter full
    parseFilter (Item _ _ (ItemChain _ (Categorial f' :| [])) mvrs) = f == f' && Map.null mvrs
    parseFilter _ = False

addChartItem :: Ord f => Chart f -> Item f -> Chart f
addChartItem c i@(Item _ _ (ItemChain (l,r) _) mvrs)
    = Chart { leftEdges  = Mmap.prepend l i $ leftEdges  c
            , rightEdges = Mmap.prepend r i $ rightEdges c
            , movers     = movers'
            , allItems   = Set.insert i $ allItems c
            }
  where
    movers' = foldr (`Mmap.prepend` i) (movers c) (Map.keys mvrs)

initialAgenda:: Grammar f String -> [String] -> Agenda f
initialAgenda g s = Agenda (empties ++ lexItems)
  where
    empties = [ Item OAxiom True
                (ItemChain (i,i) (lexItemFeatures li))
                Map.empty
              | i  <- [0..length s]
              , li <- emptyItems g
              ]
    paired = zip s [0..]
    lexItems = paired >>= \(x,i) -> map (\li -> Item OAxiom True (ItemChain (i,i+1) (lexItemFeatures li)) Map.empty) $ valueItems g x


recognize :: (Eq f, Ord f) => Grammar f String -> [String] -> Bool
recognize g s =
  isChartDone (fillChart g s) (startCategory g) (length s)

fillChart :: (Eq f, Ord f) => Grammar f String -> [String] -> Chart f
fillChart g s =
    runST (do
        chart  <- newSTRef emptyChart
        agenda <- newSTRef $ initialAgenda g s
        _ <- iterateUntil isEmptyAgenda
                 (do
                     agenda' <- readSTRef agenda
                     chart'  <- readSTRef chart
                     let (item, rest) = fromJust $ nextAgendaItem agenda'
                     writeSTRef agenda $ Agenda (rest ++ merge1All chart' item
                                                      ++ merge2All chart' item
                                                      ++ merge3All chart' item
                                                      ++ move1 item
                                                      ++ move2 item)
                     writeSTRef chart  $ addChartItem chart' item
                     readSTRef agenda)
        readSTRef chart)

merge1All :: (Eq f, Ord f) =>  Chart f -> Item f -> [Item f]
merge1All c i@(Item _ _ (ItemChain (p,q) _) _) =
  mapMaybe (`merge1` i) $ (Set.toList $ q `Mmap.find` leftEdges c) ++ (Set.toList $ p `Mmap.find` rightEdges c)

merge1 :: Eq f => Item f -> Item f -> Maybe (Item f)
merge1 (Item _ True (ItemChain (p,q1) (Selectional f1 :| fs)) _)
       (Item _ _    (ItemChain (q2,v) (Categorial  f2 :| [])) mvrs)
  | f1 == f2
  , q1 == q2
    = Just (Item OMerge1 False (ItemChain (p,v) (NonEmpty.fromList fs)) mvrs)
merge1 (Item _ _    (ItemChain (q2,v) (Categorial  f2 :| [])) mvrs)
       (Item _ True (ItemChain (p,q1) (Selectional f1 :| fs)) _)
  | f1 == f2
  , q1 == q2
    = Just (Item OMerge1 False (ItemChain (p,v) (NonEmpty.fromList fs)) mvrs)
merge1 _ _ = Nothing

merge2All :: (Eq f, Ord f) =>  Chart f -> Item f -> [Item f]
merge2All c i@(Item _ _ (ItemChain (p,q) _) _) =
  mapMaybe (`merge2` i) $ (Set.toList $ q `Mmap.find` leftEdges c) ++ (Set.toList $ p `Mmap.find` rightEdges c)

merge2 :: (Eq f, Ord f) => Item f -> Item f -> Maybe (Item f)
merge2 (Item _ False (ItemChain (p1,q) (Selectional f1 :| fs)) mvrs1)
       (Item _ _     (ItemChain (v,p2) (Categorial  f2 :| [])) mvrs2)
  | f1 == f2
  , p1 == p2
  , Map.null $ Map.intersection mvrs1 mvrs2
    = Just (Item OMerge2 False (ItemChain (v,q) (NonEmpty.fromList fs)) (Map.union mvrs1 mvrs2))
merge2 (Item _ _     (ItemChain (v,p2) (Categorial  f2 :| [])) mvrs2)
       (Item _ False (ItemChain (p1,q) (Selectional f1 :| fs)) mvrs1)
  | f1 == f2
  , p1 == p2
  , Map.null $ Map.intersection mvrs1 mvrs2
    = Just (Item OMerge2 False (ItemChain (v,q) (NonEmpty.fromList fs)) (Map.union mvrs1 mvrs2))
merge2 _ _ = Nothing

merge3All :: (Eq f, Ord f) => Chart f -> Item f -> [Item f]
merge3All c i =
  mapMaybe (`merge3` i) $ (Set.toList $ allItems c)

merge3 :: (Eq f, Ord f) => Item f -> Item f -> Maybe (Item f)
merge3 (Item _ _ (ItemChain (p,q) (Selectional f1 :| fs)) mvrs1)
       (Item _ _ (ItemChain (v,w) (Categorial  f2 :| (Licensee f:fs'))) mvrs2)
  | f1 == f2
  , Map.null $ Map.intersection (Map.singleton f (ItemChain (v,w) (Licensee f :| fs'))) $ Map.intersection mvrs1 mvrs2
    = Just (Item OMerge3 False (ItemChain (p,q) (NonEmpty.fromList fs)) (Map.unions [mvrs1,mvrs2, Map.singleton f (ItemChain (v,w) (Licensee f :| fs'))]))
merge3 (Item _ _ (ItemChain (v,w) (Categorial  f2 :| (Licensee f:fs'))) mvrs2)
       (Item _ _ (ItemChain (p,q) (Selectional f1 :| fs)) mvrs1)
  | f1 == f2
  , Map.null $ Map.intersection (Map.singleton f (ItemChain (v,w) (Licensee f :| fs'))) $ Map.intersection mvrs1 mvrs2
    = Just (Item OMerge3 False (ItemChain (p,q) (NonEmpty.fromList fs)) (Map.unions [mvrs1,mvrs2, Map.singleton f (ItemChain (v,w) (Licensee f :| fs'))]))
merge3 _ _ = Nothing

move1 :: (Eq f, Ord f) => Item f -> [Item f]
move1 (Item _ False (ItemChain (p1,q) (Licenser f :| fs)) mvrs)
    = [ Item OMove1 False (ItemChain (v,q) (NonEmpty.fromList fs)) mvrs'
      | ItemChain (v,p2) fs' <- mvr
      , p1 == p2
      , length fs' == 1
      ]
  where
    mvr   = maybeToList $ f `Map.lookup` mvrs
    mvrs' = f `Map.delete` mvrs
move1 _ = []

move2 :: (Eq f, Ord f) => Item f -> [Item f]
move2 (Item _ False (ItemChain (p,q) (Licenser f :| fs)) mvrs)
    = [ Item OMove2 False (ItemChain (p,q) (NonEmpty.fromList fs)) $
        Map.insert (feat fs') (ItemChain (v,w) (NonEmpty.fromList fs')) mvrs'
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
showItem (Item op True mainChain ms) =
  show op ++ " [" ++ showItemChain mainChain ++ (Map.elems ms >>= showItemChain) ++ "]s\n"
showItem (Item op False mainChain ms) =
  show op ++ " [" ++ showItemChain mainChain ++ (Map.elems ms >>= showItemChain) ++ "]c\n"

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
