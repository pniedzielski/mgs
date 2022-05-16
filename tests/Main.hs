module Main where

import Test.Tasty (testGroup)
import Test.Tasty.Travis (travisTestReporter, defaultConfig)

import qualified FormalLanguage.MG.Feature.Tests

main ∷ IO ()
main = travisTestReporter defaultConfig [] $ testGroup "Tests"
  [ FormalLanguage.MG.Feature.Tests.tests
  ]
