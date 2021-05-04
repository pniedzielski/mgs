{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE OverloadedStrings    #-}

module Text.MG.Feature
  ( Feature(..)
  , renderFeature
  ) where

import Data.Text( Text )

data Feature f
    = Categorial f
    | Selectional f
    | Licenser f
    | Licensee f
  deriving (Eq, Ord, Show, Functor)

renderFeature ∷ Feature Text → Text
renderFeature (Categorial f)  = f
renderFeature (Selectional f) = "=" <> f
renderFeature (Licenser f)    = "+" <> f
renderFeature (Licensee f)    = "-" <> f
