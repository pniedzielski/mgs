module Main where

import           Test.Tasty (defaultMain, testGroup)

import qualified Text.MG.Feature.Tests

main ∷ IO ()
main = defaultMain $ testGroup "Tests"
    [ Text.MG.Feature.Tests.tests
    ]
