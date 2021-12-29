module Main (main) where

import Data.Maybe ( fromJust )
import qualified Data.Tree
import qualified Data.Tree.View
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Megaparsec ( parseMaybe )
import Text.MG
import Text.MG.Algebra.DerivationTree
import Text.MG.IO ( formatGrammar, parseMG )
import Text.MG.Parser.BottomUp
import Prelude.Unicode
import System.Environment ( getArgs )

-------------------------------------------------------------------------------

drawTree ∷ Data.Tree.Tree T.Text → IO ()
drawTree = Data.Tree.View.drawTree ∘ fmap T.unpack

main ∷ IO ()
main = do
  args        ← getArgs
  grammarText ← TIO.readFile $ head args
  let grammar = fromJust $ parseMaybe parseMG grammarText
  TIO.putStr $ formatGrammar grammar

  str ← TIO.getLine
  let (chart', df') = fillChart grammar $ T.words str
  TIO.putStr $ showChart chart'
  let n = length ∘ T.words $ str
  let c = startCategory grammar
  drawTree ∘ derivationTree ∘ head $ concatMap (derivations df') (doneItems chart' c n)
