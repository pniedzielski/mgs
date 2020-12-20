module Main (main) where

import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NonEmpty
import Text.MG


testMe :: Grammar Char String -> String -> IO ()
testMe g s = do
    if recognize g $ words s
      then putStrLn $ s ++ ": yes!"
      else putStrLn $ s ++ ": no!"



main ∷ IO ()
main = do
  testMe g1 ""
  testMe g1 "Brutus stabbed Caesar"
  testMe g1 "Caesar stabbed Brutus"
  testMe g1 "stabbed Brutus Caesar"
  testMe g1 "Caesar Brutus stabbed"
  testMe g1 "hello"

  testMe g2 ""
  testMe g2 "Brutus stabbed Caesar"
  testMe g2 "Caesar stabbed Brutus"
  testMe g2 "stabbed Brutus Caesar"
  testMe g2 "Caesar Brutus stabbed"
  testMe g2 "hello"
  testMe g2 "Brutus did stab Caesar"
  testMe g2 "Caesar did stab Brutus"
  testMe g2 "Caesar did see Brutus"
  testMe g2 "Caesar did Brutus see"
  testMe g2 "Caesar did fall"
  testMe g2 "who did fall"
  testMe g2 "who did Brutus see"
  testMe g2 "who did see Brutus"
  putStrLn . showChart . fillChart g2 $ words "who did Brutus see"


dp1 :: String -> LexItem Char String
dp1 = LexItem $ NonEmpty.fromList [Categorial 'd']

vtr1 :: String -> LexItem Char String
vtr1 = LexItem $ NonEmpty.fromList [Selectional 'd', Categorial 'V']

g1 :: Grammar Char String
g1 = Grammar
  { startCategory = 'v'
  , lexicon = Set.fromList
    [ dp1 "Brutus"
    , dp1 "Caesar"
    , vtr1 "stabbed"
    , vtr1 "saw"
    , LexItem (NonEmpty.fromList [Selectional 'V', Selectional 'd', Categorial 'v']) ""
    ]
  }


dp2 :: String -> LexItem Char String
dp2 = LexItem $ NonEmpty.fromList [Categorial 'd', Licensee 'k']

vtr2 :: String -> LexItem Char String
vtr2 = LexItem $ NonEmpty.fromList [Selectional 'd', Categorial 'V', Licensee 'v']

g2 :: Grammar Char String
g2 = Grammar
  { startCategory = 'c'
  , lexicon = Set.fromList
    [ dp2 "Brutus"
    , dp2 "Caesar"
    , vtr2 "stab"
    , vtr2 "see"
    , LexItem (NonEmpty.fromList [Selectional 'd', Categorial 'V', Licensee 'v']) "fall"
    , LexItem (NonEmpty.fromList [Selectional 'V', Licenser 'k', Licenser 'v', Selectional 'd', Categorial 'v', Licensee 't']) ""
    , LexItem (NonEmpty.fromList [Selectional 'V', Licenser 'v', Categorial 'v', Licensee 't']) ""
    , LexItem (NonEmpty.fromList [Selectional 'v', Licenser 'k', Categorial 't']) "did"
    , LexItem (NonEmpty.fromList [Selectional 'v', Licenser 'k', Categorial 't', Licensee 'c']) "did"
    , LexItem (NonEmpty.fromList [Selectional 't', Categorial 'c']) ""
    , LexItem (NonEmpty.fromList [Selectional 't', Licenser 'c', Licenser 'w', Categorial 'c']) ""
    , LexItem (NonEmpty.fromList [Categorial 'd', Licensee 'k', Licensee 'w']) "who"
    ]
  }
