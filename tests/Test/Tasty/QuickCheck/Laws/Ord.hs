module Test.Tasty.QuickCheck.Laws.Ord
  ( testOrdLaws
  , testOrdLawReflexive
  , testOrdLawAntisymmetric
  , testOrdLawTransitive
  , testOrdLawLessEqualDual
  , testOrdLawLessTrichotomy
  , testOrdLawLessDual
  , testOrdLawLessIsLT
  , testOrdLawGreaterIsGT
  , testOrdLawEqualIsEQ
  , testOrdLawMin
  , testOrdLawMax
  ) where

import Data.Proxy (Proxy)
import Data.Typeable (Typeable, typeRep)
import Prelude.Unicode
import Test.Tasty
import Test.Tasty.QuickCheck

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
