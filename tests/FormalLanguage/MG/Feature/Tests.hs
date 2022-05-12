{-# OPTIONS_GHC -fno-warn-orphans #-}
module FormalLanguage.MG.Feature.Tests
    ( tests
    ) where

import FormalLanguage.MG.Feature
import Prelude.Unicode
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

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

-------------------------------------------------------------------------------
tests ∷ TestTree
tests = testGroup "FormalLanguage.MG.Feature"
  [ testCategorial
  , testSelectional
  , testLicenser
  , testLicensee
  , testOrd
  , testBounded
  , testEnum
  , testPolarity
  , testOperation
  , testMatching
  ]

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
  [ testProperty "Features are positive xor negative"
      (propPosXorNeg ∷ Feature Integer → Bool)
  , testProperty "Positive features have Pos polarity"
      (propPos ∷ Feature Integer → Bool)
  , testProperty "Negative features have Neg polarity"
      (propNeg ∷ Feature Integer → Bool)
  ]

propPosXorNeg ∷ Feature f → Bool
propPosXorNeg f = pos f ≢ neg f

propPos ∷ Feature f → Bool
propPos f = (polarity f ≡ Pos) ≡ pos f

propNeg ∷ Feature f → Bool
propNeg f = (polarity f ≡ Neg) ≡ neg f

-------------------------------------------------------------------------------
testOrd ∷ TestTree
testOrd = testGroup "Ord (Feature f)"
  [ testProperty "Reflexivity"
      (propOrdReflexive ∷ Feature Int → Bool)
  , testProperty "Antisymmetric" -- Bool to reduce search space
      (propOrdAntisymmetric ∷ Feature Bool → Feature Bool → Property)
  , testProperty "Transitive"
      (propOrdTransitive ∷ Feature Int → Feature Int → Feature Int → Property)
  , testProperty "≥ is the dual of ≤"
      (propLessEqualDual ∷ Feature Int → Feature Int → Bool)
  , testProperty "< means ≤ and not ≡"
      (propLessTrichotomy ∷ Feature Int → Feature Int → Bool)
  , testProperty "> is the dual of <"
      (propLessDual ∷ Feature Int → Feature Int → Bool)
  , testProperty "< when compare returns LT"
      (propLessIsLT ∷ Feature Int → Feature Int → Bool)
  , testProperty "> when compare returns GT"
      (propGreaterIsGT ∷ Feature Int → Feature Int → Bool)
  , testProperty "≡ when compare returns EQ"
      (propEqualIsEQ ∷ Feature Int → Feature Int → Bool)
  , testProperty "min is the smaller of its arguments"
      (propMin ∷ Feature Int → Feature Int → Bool)
  , testProperty "max is the larger of its arguments"
      (propMax ∷ Feature Int → Feature Int → Bool)
  ]

propOrdReflexive ∷ Ord f ⇒ Feature f → Bool
propOrdReflexive f = f ≤ f

propOrdAntisymmetric ∷ Ord f ⇒ Feature f → Feature f → Property
propOrdAntisymmetric f1 f2 = f1 ≤ f2 ∧ f1 ≥ f2 ==> f1 ≡ f2

propOrdTransitive ∷ Ord f ⇒ Feature f → Feature f → Feature f → Property
propOrdTransitive f1 f2 f3 = f1 ≤ f2 ∧ f2 ≤ f3 ==> f1 ≤ f3

propLessEqualDual ∷ Ord f ⇒ Feature f → Feature f → Bool
propLessEqualDual f1 f2 = (f1 ≥ f2) ≡ (f2 ≤ f1)

propLessTrichotomy ∷ Ord f ⇒ Feature f → Feature f → Bool
propLessTrichotomy f1 f2 = (f1 < f2) ≡ (f1 ≤ f2 ∧ f1 ≢ f2)

propLessDual ∷ Ord f ⇒ Feature f → Feature f → Bool
propLessDual f1 f2 = (f1 > f2) ≡ (f2 < f1)

propLessIsLT ∷ Ord f ⇒ Feature f → Feature f → Bool
propLessIsLT f1 f2 = (f1 < f2) ≡ (f1 `compare` f2 ≡ LT)

propGreaterIsGT ∷ Ord f ⇒ Feature f → Feature f → Bool
propGreaterIsGT f1 f2 = (f1 > f2) ≡ (f1 `compare` f2 ≡ GT)

propEqualIsEQ ∷ Ord f ⇒ Feature f → Feature f → Bool
propEqualIsEQ f1 f2 = (f1 ≡ f2) ≡ (f1 `compare` f2 ≡ EQ)

propMin ∷ Ord f ⇒ Feature f → Feature f → Bool
propMin f1 f2 = f1 `min` f2 ≡ if f1 ≤ f2 then f1 else f2

propMax ∷ Ord f ⇒ Feature f → Feature f → Bool
propMax f1 f2 = f1 `max` f2 ≡ if f1 ≥ f2 then f1 else f2

-------------------------------------------------------------------------------
testBounded ∷ TestTree
testBounded = testGroup "Bounded (Feature f)"
  [ testCase "minBound < maxBound" testMinLessMax
  , testProperty "maxBound is the largest value"
      (propMaxBoundMax ∷ Feature Int → Bool)
  , testProperty "minBound is the smallest value"
      (propMinBoundMin ∷ Feature Int → Bool)
  ]

testMinLessMax ∷ Assertion
testMinLessMax =
  (minBound ∷ Feature Int) < (maxBound ∷ Feature Int) @? "minBound ≥ maxBound"

propMaxBoundMax ∷ (Ord f, Bounded f) ⇒ Feature f → Bool
propMaxBoundMax f = f ≤ maxBound

propMinBoundMin ∷ (Ord f, Bounded f) ⇒ Feature f → Bool
propMinBoundMin f = f ≥ minBound

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
testOperation ∷ TestTree
testOperation = testGroup "Operation"
  [ testProperty "Features are merge xor move"
      (propMoveXorMerge ∷ Feature Integer → Bool)
  , testProperty "Movement features have Move polarity"
      (propMove ∷ Feature Integer → Bool)
  , testProperty "Merge features have Merge polarity"
      (propMerge ∷ Feature Integer → Bool)
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
