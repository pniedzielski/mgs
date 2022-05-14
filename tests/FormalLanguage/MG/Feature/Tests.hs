{-# LANGUAGE ScopedTypeVariables  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module FormalLanguage.MG.Feature.Tests
    ( tests
    ) where

import Data.Proxy (Proxy(Proxy))
import Data.Typeable (Typeable, typeRep)
import FormalLanguage.MG.Feature
import Prelude.Unicode
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.QuickCheck.Laws

-------------------------------------------------------------------------------
instance Arbitrary1 Feature where
  liftArbitrary f = oneof [ Categorial  <$> f
                          , Selectional <$> f
                          , Licenser    <$> f
                          , Licensee    <$> f
                          ]

instance Arbitrary f ⇒ Arbitrary (Feature f) where
  arbitrary = arbitrary1
  shrink    = shrink1

instance Arbitrary (Polarity) where
  arbitrary = elements [ Pos, Neg ]

instance Arbitrary (Operation) where
  arbitrary = elements [ OpMerge, OpMove ]

-------------------------------------------------------------------------------
tests ∷ TestTree
tests = testGroup "FormalLanguage.MG.Feature"
  [ testFeature
  , testPolarity
  , testOperation
  , testMatching
  ]

-------------------------------------------------------------------------------
testFeature ∷ TestTree
testFeature = testGroup "Feature"
  [ testFeatureLaws
  , testCategorial
  , testSelectional
  , testLicenser
  , testLicensee
  ]

-------------------------------------------------------------------------------
testFeatureLaws ∷ TestTree
testFeatureLaws = testGroup "Typeclass Laws"
  [ testEqLaws  (Proxy ∷ Proxy (Feature ()))
  , testEqLaws  (Proxy ∷ Proxy (Feature Bool))
  , testEqLaws  (Proxy ∷ Proxy (Feature String))
  , testOrdLaws (Proxy ∷ Proxy (Feature ()))
  , testOrdLaws (Proxy ∷ Proxy (Feature Bool))
  , testBounded
  , testBoundedOrdLaws (Proxy ∷ Proxy (Feature ()))
  , testBoundedOrdLaws (Proxy ∷ Proxy (Feature Bool))
  , testBoundedOrdLaws (Proxy ∷ Proxy (Feature Int))
  , testEnum
  ]

-------------------------------------------------------------------------------
testOrdLaws
  ∷ (Ord α, Show α, Arbitrary α, Typeable α)
  ⇒ Proxy α
  → TestTree
testOrdLaws pa =
  let name = "Ord Laws for " ++ (show $ typeRep pa) in
    testGroup name
    [ testOrdLawReflexive      pa
    , testOrdLawAntisymmetric  pa
    , testOrdLawTransitive     pa
    , testOrdLawLessEqualDual  pa
    , testOrdLawLessTrichotomy pa
    , testOrdLawLessDual       pa
    , testOrdLawLessIsLT       pa
    , testOrdLawGreaterIsGT    pa
    , testOrdLawEqualIsEQ      pa
    , testOrdLawMin            pa
    , testOrdLawMax            pa
    ]

-- | @a ≤ a@
testOrdLawReflexive
  ∷ (Ord α, Show α, Arbitrary α)
  ⇒ Proxy α
  → TestTree
testOrdLawReflexive pa =
  testProperty "a ≤ a" $
    ordLawReflexive pa

ordLawReflexive
  ∷ Ord α
  ⇒ Proxy α
  → α → Bool
ordLawReflexive _ a =
  a ≤ a

-- | @a ≤ b ∧ a ≥ b ==> a ≡ b@
testOrdLawAntisymmetric
  ∷ (Ord α, Show α, Arbitrary α)
  ⇒ Proxy α
  → TestTree
testOrdLawAntisymmetric pa =
  testProperty "a ≤ b ∧ a ≥ b ==> a ≡ b" $
    ordLawAntisymmetric pa

ordLawAntisymmetric
  ∷ Ord α
  ⇒ Proxy α
  → α → α → Property
ordLawAntisymmetric _ a b =
  a ≤ b ∧ a ≥ b ==> a ≡ b

-- | @a ≤ b ∧ b ≤ c ==> a ≤ c@
testOrdLawTransitive
  ∷ (Ord α, Show α, Arbitrary α)
  ⇒ Proxy α
  → TestTree
testOrdLawTransitive pa =
  testProperty "a ≤ b ∧ b ≤ c ==> a ≤ c" $
    ordLawTransitive pa

ordLawTransitive
  ∷ Ord α
  ⇒ Proxy α
  → α → α → α → Property
ordLawTransitive _ a b c =
  a ≤ b ∧ b ≤ c ==> a ≤ c

-- | @(a ≥ b) ≡ (b ≤ a)@
testOrdLawLessEqualDual
  ∷ (Ord α, Show α, Arbitrary α)
  ⇒ Proxy α
  → TestTree
testOrdLawLessEqualDual pa =
  testProperty "(a ≥ b) ≡ (b ≤ a)" $
    ordLawLessEqualDual pa

ordLawLessEqualDual
  ∷ Ord α
  ⇒ Proxy α
  → α → α → Bool
ordLawLessEqualDual _ a b =
  (a ≥ b) ≡ (b ≤ a)

-- | @(a < b) ≡ (a ≤ b ∧ a ≢ b)@
testOrdLawLessTrichotomy
  ∷ (Ord α, Show α, Arbitrary α)
  ⇒ Proxy α
  → TestTree
testOrdLawLessTrichotomy pa =
  testProperty "(a < b) ≡ (a ≤ b ∧ a ≢ b)" $
    ordLawLessTrichotomy pa

ordLawLessTrichotomy
  ∷ Ord α
  ⇒ Proxy α
  → α → α → Bool
ordLawLessTrichotomy _ a b =
  (a < b) ≡ (a ≤ b ∧ a ≢ b)

-- | @(a > b) ≡ (b < a)@
testOrdLawLessDual
  ∷ (Ord α, Show α, Arbitrary α)
  ⇒ Proxy α
  → TestTree
testOrdLawLessDual pa =
  testProperty "(a > b) ≡ (b < a)" $
    ordLawLessDual pa

ordLawLessDual
  ∷ Ord α
  ⇒ Proxy α
  → α → α → Bool
ordLawLessDual _ a b =
  (a > b) ≡ (b < a)

-- | @(a < b) ≡ (a `compare` b ≡ LT)@
testOrdLawLessIsLT
  ∷ (Ord α, Show α, Arbitrary α)
  ⇒ Proxy α
  → TestTree
testOrdLawLessIsLT pa =
  testProperty "(a < b) ≡ (a `compare` b ≡ LT)" $
    ordLawLessIsLT pa

ordLawLessIsLT
  ∷ Ord α
  ⇒ Proxy α
  → α → α → Bool
ordLawLessIsLT _ a b =
  (a < b) ≡ (a `compare` b ≡ LT)

-- | @(a > b) ≡ (a `compare` b ≡ GT)@
testOrdLawGreaterIsGT
  ∷ (Ord α, Show α, Arbitrary α)
  ⇒ Proxy α
  → TestTree
testOrdLawGreaterIsGT pa =
  testProperty "(a > b) ≡ (a `compare` b ≡ GT)" $
    ordLawGreaterIsGT pa

ordLawGreaterIsGT
  ∷ Ord α
  ⇒ Proxy α
  → α → α → Bool
ordLawGreaterIsGT _ a b =
  (a > b) ≡ (a `compare` b ≡ GT)

-- | @(a ≡ b) ≡ (a `compare` b ≡ EQ)@
testOrdLawEqualIsEQ
  ∷ (Ord α, Show α, Arbitrary α)
  ⇒ Proxy α
  → TestTree
testOrdLawEqualIsEQ pa =
  testProperty "(a ≡ b) ≡ (a `compare` b ≡ EQ)" $
    ordLawEqualIsEQ pa

ordLawEqualIsEQ
  ∷ Ord α
  ⇒ Proxy α
  → α → α → Bool
ordLawEqualIsEQ _ a b =
  (a ≡ b) ≡ (a `compare` b ≡ EQ)

-- | @a `min` b ≡ if a ≤ b then a else b@
testOrdLawMin
  ∷ (Ord α, Show α, Arbitrary α)
  ⇒ Proxy α
  → TestTree
testOrdLawMin pa =
  testProperty "a `min` b ≡ if a ≤ b then a else b" $
    ordLawMin pa

ordLawMin
  ∷ Ord α
  ⇒ Proxy α
  → α → α → Bool
ordLawMin _ a b =
  a `min` b ≡ if a ≤ b then a else b

-- | @a `max` b ≡ if a ≥ b then a else b@
testOrdLawMax
  ∷ (Ord α, Show α, Arbitrary α)
  ⇒ Proxy α
  → TestTree
testOrdLawMax pa =
  testProperty "a `max` b ≡ if a ≥ b then a else b" $
    ordLawMax pa

ordLawMax
  ∷ Ord α
  ⇒ Proxy α
  → α → α → Bool
ordLawMax _ a b =
  a `max` b ≡ if a ≥ b then a else b

-------------------------------------------------------------------------------
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

-------------------------------------------------------------------------------
testEnum ∷ TestTree
testEnum = testGroup "Enum (Feature f)"
  [ testProperty "toEnum ∘ fromEnum is the identity function"
      (propEnumToFromId ∷ Feature Int → Bool)
  , testProperty "toEnum is a monotone function" propEnumToMonotone
  , testProperty "fromEnum is a monotone function"
      (propEnumFromMonotone ∷ Feature Int → Feature Int → Bool)
  ]

propEnumToFromId ∷ (Eq f, Enum f) ⇒ Feature f → Bool
propEnumToFromId f = f ≡ (toEnum ∘ fromEnum $ f)

propEnumToMonotone ∷ (NonNegative Int) → (NonNegative Int) → Bool
propEnumToMonotone (NonNegative f1) (NonNegative f2)
  | f1 ≤ f2   = (toEnum f1 ∷ Feature Int) ≤ (toEnum f2 ∷ Feature Int)
  | otherwise = (toEnum f1 ∷ Feature Int) > (toEnum f2 ∷ Feature Int)

propEnumFromMonotone ∷ (Ord f, Enum f) ⇒ Feature f → Feature f → Bool
propEnumFromMonotone f1 f2
  | f1 ≤ f2   = fromEnum f1 ≤ fromEnum f2
  | otherwise = fromEnum f1 > fromEnum f2

-------------------------------------------------------------------------------
testCategorial ∷ TestTree
testCategorial = testGroup "Categorial"
  [ testCase "basicFeature" $ testCategorial_basicFeature "feat"
  , testCase "polarity"     $ testCategorial_polarity "feat"
  , testCase "operation"    $ testCategorial_operation "feat"
  ]

testCategorial_basicFeature ∷ (Eq f, Show f) ⇒ f → Assertion
testCategorial_basicFeature f =
  (basicFeature ∘ Categorial $ f) @=? f

testCategorial_polarity ∷ f → Assertion
testCategorial_polarity f =
  (polarity ∘ Categorial $ f) @=? Neg

testCategorial_operation ∷ f → Assertion
testCategorial_operation f =
  (operation ∘ Categorial $ f) @=? OpMerge

-------------------------------------------------------------------------------
testSelectional ∷ TestTree
testSelectional = testGroup "Selectional"
  [ testCase "basicFeature" $ testSelectional_basicFeature "feat"
  , testCase "polarity"     $ testSelectional_polarity "feat"
  , testCase "operation"    $ testSelectional_operation "feat"
  ]

testSelectional_basicFeature ∷ (Eq f, Show f) ⇒ f → Assertion
testSelectional_basicFeature f =
  (basicFeature ∘ Selectional $ f) @=? f

testSelectional_polarity ∷ f → Assertion
testSelectional_polarity f =
  (polarity ∘ Selectional $ f) @=? Pos

testSelectional_operation ∷ f → Assertion
testSelectional_operation f =
  (operation ∘ Selectional $ f) @=? OpMerge

-------------------------------------------------------------------------------
testLicenser ∷ TestTree
testLicenser = testGroup "Licenser"
  [ testCase "basicFeature" $ testLicenser_basicFeature "feat"
  , testCase "polarity"     $ testLicenser_polarity "feat"
  , testCase "operation"    $ testLicenser_operation "feat"
  ]

testLicenser_basicFeature ∷ (Eq f, Show f) ⇒ f → Assertion
testLicenser_basicFeature f =
  (basicFeature ∘ Licenser $ f) @=? f

testLicenser_polarity ∷ f → Assertion
testLicenser_polarity f =
  (polarity ∘ Licenser $ f) @=? Pos

testLicenser_operation ∷ f → Assertion
testLicenser_operation f =
  (operation ∘ Licenser $ f) @=? OpMove

-------------------------------------------------------------------------------
testLicensee ∷ TestTree
testLicensee = testGroup "Licensee"
  [ testCase "basicFeature" $ testLicensee_basicFeature "feat"
  , testCase "polarity"     $ testLicensee_polarity "feat"
  , testCase "operation"    $ testLicensee_operation "feat"
  ]

testLicensee_basicFeature ∷ (Eq f, Show f) ⇒ f → Assertion
testLicensee_basicFeature f =
  (basicFeature ∘ Licensee $ f) @=? f

testLicensee_polarity ∷ f → Assertion
testLicensee_polarity f =
  (polarity ∘ Licensee $ f) @=? Neg

testLicensee_operation ∷ f → Assertion
testLicensee_operation f =
  (operation ∘ Licensee $ f) @=? OpMove

-------------------------------------------------------------------------------
testPolarity ∷ TestTree
testPolarity = testGroup "Polarity"
  [ testPolarityLaws
  , testProperty "Features are positive xor negative"
      (propPosXorNeg ∷ Feature Integer → Bool)
  , testProperty "Positive features have Pos polarity"
      (propPos ∷ Feature Integer → Bool)
  , testProperty "Negative features have Neg polarity"
      (propNeg ∷ Feature Integer → Bool)
  ]

testPolarityLaws ∷ TestTree
testPolarityLaws = testGroup "Typeclass Laws"
  [ testEqLaws  (Proxy ∷ Proxy Polarity)
  , testOrdLaws (Proxy ∷ Proxy Polarity)
  ]

propPosXorNeg ∷ Feature f → Bool
propPosXorNeg f = pos f ≢ neg f

propPos ∷ Feature f → Bool
propPos f = (polarity f ≡ Pos) ≡ pos f

propNeg ∷ Feature f → Bool
propNeg f = (polarity f ≡ Neg) ≡ neg f

-------------------------------------------------------------------------------
testOperation ∷ TestTree
testOperation = testGroup "Operation"
  [ testOperationLaws
  , testProperty "Features are merge xor move"
      (propMoveXorMerge ∷ Feature Integer → Bool)
  , testProperty "Movement features have Move polarity"
      (propMove ∷ Feature Integer → Bool)
  , testProperty "Merge features have Merge polarity"
      (propMerge ∷ Feature Integer → Bool)
  ]

testOperationLaws ∷ TestTree
testOperationLaws = testGroup "Typeclass Laws"
  [ testEqLaws  (Proxy ∷ Proxy Operation)
  , testOrdLaws (Proxy ∷ Proxy Operation)
  ]

propMoveXorMerge ∷ Feature f → Bool
propMoveXorMerge f = opMerge f ≢ opMove f

propMerge ∷ Feature f → Bool
propMerge f = (operation f ≡ OpMerge) ≡ opMerge f

propMove ∷ Feature f → Bool
propMove f = (operation f ≡ OpMove) ≡ opMove f

-------------------------------------------------------------------------------
testMatching ∷ TestTree
testMatching = testGroup "Featural Matching"
  [ testProperty "Matching features match"
      (propMatchingFeatureMatches ∷ Feature Integer → Bool)
  , testProperty "Matching features differ in polarity"
      (propMatchingFeaturePolarities ∷ Feature Integer → Bool)
  , testProperty "Matching features share their operation"
      (propMatchingFeatureOperations ∷ Feature Integer → Bool)
  , testProperty "Matching features share their basic feature"
      (propMatchingFeatureBasicFeature ∷ Feature Integer → Bool)
  ]

propMatchingFeatureMatches ∷ Eq f ⇒ Feature f → Bool
propMatchingFeatureMatches f =
  matchingFeature f `matches` f ∧ f `matches` matchingFeature f

propMatchingFeaturePolarities ∷ Feature f → Bool
propMatchingFeaturePolarities f =
  polarity f ≢ polarity f'
  where f' = matchingFeature f

propMatchingFeatureOperations ∷ Feature f → Bool
propMatchingFeatureOperations f =
  operation f ≡ operation f'
  where f' = matchingFeature f

propMatchingFeatureBasicFeature ∷ Eq f ⇒ Feature f → Bool
propMatchingFeatureBasicFeature f =
  basicFeature f ≡ basicFeature f'
  where f' = matchingFeature f
