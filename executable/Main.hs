{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main (main) where

import           Control.Recursion( Fix(..) )
import           Data.List.NonEmpty( NonEmpty(..) )
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
  TIO.putStrLn ∘ renderLexItem $ li1
  TIO.putStrLn ∘ renderLexItem $ li2
  TIO.putStrLn ∘ renderDerivation $ deriv3
  putDoc ∘ PP.pretty $ deriv4
  TIO.putStrLn ""
  putStrLn ∘ Tree.drawTree ∘ treeDerivation $ deriv4

li1, li2 ∷ LexItem T.Text T.Text
li1 = LexItem (Selectional "a" :| [Categorial "b"]) "a"
li2 = LexItem (Categorial "c" :| []) "b"

deriv1, deriv2, deriv3, deriv4 ∷ Derivation T.Text T.Text
deriv1 = Fix (Select li1)
deriv2 = Fix (Select li2)
deriv3 = Fix (Merge1 deriv1 deriv2)
deriv4 = Fix (Merge2 deriv3 (Fix (Merge2 deriv3 deriv3)))


