module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified FormalLanguage.MG.Feature.Tests

main ∷ IO ()
main = defaultMain $ testGroup "Tests"
  [ FormalLanguage.MG.Feature.Tests.tests
  ]
