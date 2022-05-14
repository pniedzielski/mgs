{-# LANGUAGE ScopedTypeVariables  #-}
module Test.Tasty.QuickCheck.Laws.BoundedOrd
  ( testBoundedOrdLaws
  , testBoundedOrdLawMinLessMax
  , testBoundedOrdLawMaxBoundMax
  , testBoundedOrdLawMinBoundMin
  ) where

import Data.Proxy (Proxy)
import Data.Typeable (Typeable, typeRep)
import Prelude.Unicode
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

testBoundedOrdLaws
  ∷ (Bounded α, Ord α, Show α, Arbitrary α, Typeable α)
  ⇒ Proxy α
  → TestTree
testBoundedOrdLaws pa =
  let name = "Bounded and Ord Laws for " ++ (show $ typeRep pa) in
    testGroup name
    [ testBoundedOrdLawMinLessMax  pa
    , testBoundedOrdLawMaxBoundMax pa
    , testBoundedOrdLawMinBoundMin pa
    ]

-- | @minBound > maxBound@
testBoundedOrdLawMinLessMax
  ∷ (Bounded α, Ord α, Show α)
  ⇒ Proxy α
  → TestTree
testBoundedOrdLawMinLessMax pa =
  testCase "minBound ≤ maxBound" $
    boundedOrdLawMinLessMax pa @? "minBound > maxBound"

boundedOrdLawMinLessMax
  ∷ ∀α. (Bounded α, Ord α)
  ⇒ Proxy α
  → Bool
boundedOrdLawMinLessMax _ =
  (minBound ∷ α) ≤ (maxBound ∷ α)

-- | @a ≤ maxBound@
testBoundedOrdLawMaxBoundMax
  ∷ (Bounded α, Ord α, Show α, Arbitrary α)
  ⇒ Proxy α
  → TestTree
testBoundedOrdLawMaxBoundMax pa =
  testProperty "a ≤ maxBound" $
    boundedOrdLawMaxBoundMax pa

boundedOrdLawMaxBoundMax
  ∷ (Bounded α, Ord α)
  ⇒ Proxy α
  → α → Bool
boundedOrdLawMaxBoundMax _ a =
  a ≤ maxBound

-- | @minBound ≤ a@
testBoundedOrdLawMinBoundMin
  ∷ (Bounded α, Ord α, Show α, Arbitrary α)
  ⇒ Proxy α
  → TestTree
testBoundedOrdLawMinBoundMin pa =
  testProperty "minBound ≤ a" $
    boundedOrdLawMinBoundMin pa

boundedOrdLawMinBoundMin
  ∷ (Bounded α, Ord α)
  ⇒ Proxy α
  → α → Bool
boundedOrdLawMinBoundMin _ a =
  minBound ≤ a
