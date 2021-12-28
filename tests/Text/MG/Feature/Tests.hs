module Text.MG.Feature.Tests( tests ) where

import           Prelude.Unicode
import           Test.Tasty              (TestTree, testGroup)
import           Test.Tasty.HUnit        (testCase)
import           Test.HUnit              (Assertion, (@?=), (@?))
import           Text.MG.Feature

tests ∷ TestTree
tests = testGroup "Text.MG.Feature"
    [ testCase "testBasicFeature"  testBasicFeature
    , testCase "testMergeable"     testMergeable
    , testCase "testMoveable"      testMoveable
--    , testProperty "mergeable `xor` moveable" (mergeableXorMoveable ∷ Feature Int → Bool)
    , testCase "testPos"           testPos
    , testCase "testNeg"           testNeg
--    , testProperty "pos `xor` neg" (posXorNeg ∷ Feature Int → Bool)
    ]

testBasicFeature ∷ Assertion
testBasicFeature = do
    (basicFeature ∘ Categorial)  True  @?= True
    (basicFeature ∘ Categorial)  False @?= False
    (basicFeature ∘ Selectional) True  @?= True
    (basicFeature ∘ Selectional) False @?= False
    (basicFeature ∘ Licensee)    True  @?= True
    (basicFeature ∘ Licensee)    False @?= False
    (basicFeature ∘ Licenser)    True  @?= True
    (basicFeature ∘ Licenser)    False @?= False

testMergeable ∷ Assertion
testMergeable = do
    mergeable (Categorial ())     @? "Categorial features should be mergeable"
    mergeable (Selectional ())    @? "Selectional features should be mergeable"
    not (mergeable (Licenser ())) @? "Licenser features should not be mergeable"
    not (mergeable (Licensee ())) @? "Licensee features should not be mergeable"

testMoveable ∷ Assertion
testMoveable = do
    not (moveable (Categorial ()))  @? "Categorial features should not be moveable"
    not (moveable (Selectional ())) @? "Selectional features should not be moveable"
    moveable (Licenser ())          @? "Licenser features should be moveable"
    moveable (Licensee ())          @? "Licensee features should be moveable"

-- mergeableXorMoveable ∷ Feature f → Bool
-- mergeableXorMoveable f =
--       mergeable f `xor` moveable  f
--     ∧ moveable  f `xor` mergeable f

testPos ∷ Assertion
testPos = do
    pos (Selectional ())      @? "Categorial features should be positive"
    not (pos (Categorial ())) @? "Selectional features should not be positive"
    pos (Licenser ())         @? "Licenser features should be positive"
    not (pos (Licensee ()))   @? "Licensee features should not be positive"

testNeg ∷ Assertion
testNeg = do
    not (neg (Selectional ())) @? "Categorial features should not be negative"
    neg (Categorial ())        @? "Selectional features should be negative"
    not (neg (Licenser ()))    @? "Licenser features should not be negative"
    neg (Licensee ())          @? "Licensee features should be negative"

-- posXorNeg ∷ Feature f → Bool
-- posXorNeg f =
--       pos f `xor` neg f
--     ∧ neg f `xor` pos f
