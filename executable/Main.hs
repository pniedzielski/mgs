module Main (main) where

import Data.Maybe ( fromJust )
import qualified Data.Text.IO as TIO
import Text.Megaparsec ( parseMaybe )
import Text.MG.IO ( formatGrammar, parseMG )
import System.Environment ( getArgs )

-------------------------------------------------------------------------------

main ∷ IO ()
main = do
  args        ← getArgs
  grammarText ← TIO.readFile $ head args
  let grammar = fromJust $ parseMaybe parseMG grammarText
  TIO.putStr $ formatGrammar grammar
