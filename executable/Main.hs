{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main (main) where

import           Control.Recursion( Fix(..) )
import           Data.List.NonEmpty( NonEmpty(..) )
import           Data.Maybe( fromJust )
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import qualified Data.Text.Prettyprint.Doc as PP
import           Data.Text.Prettyprint.Doc.Render.Text( putDoc )
import qualified Data.Tree                 as Tree
import           Text.Megaparsec
import           Text.MG.Derivation
import           Text.MG.Feature
import           Text.MG.Grammar
import           Prelude.Unicode

main ∷ IO ()
main = do
  parseTest (parseFeatureList <* eof) "=d +k =d  v -foc"
  parseTest (parseFeatureList <* eof) "d =n -k"
  parseTest (parseLexItem <* eof) "what∷ D -wh"
  parseTest (parseLexItem <* eof) "that ::    =T C"
  TIO.putStrLn ∘ renderLexItem $ li1
  TIO.putStrLn ∘ renderLexItem $ li2
  TIO.putStrLn ∘ renderLexItem $ li3
  TIO.putStrLn ∘ renderDerivation $ deriv4
  putDoc ∘ PP.pretty $ deriv5
  TIO.putStrLn ""
  putStrLn ∘ Tree.drawTree ∘ treeDerivation $ deriv5

li1, li2, li3 ∷ LexItem T.Text T.Text
li1 = fromJust $ parseMaybe parseLexItem "Brutus ∷ d"
li2 = fromJust $ parseMaybe parseLexItem "Caesar ∷ d"
li3 = fromJust $ parseMaybe parseLexItem "stabbed ∷ =d =d v"

deriv1, deriv2, deriv3, deriv4, deriv5 ∷ Derivation T.Text T.Text
deriv1 = Fix (Select li1)
deriv2 = Fix (Select li2)
deriv3 = Fix (Select li3)
deriv4 = Fix (Merge1 deriv3 deriv2)
deriv5 = Fix (Merge2 deriv4 deriv1)


