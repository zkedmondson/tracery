{-|
Module      : Vis
Description : Experimenting with Gloss Visualization 
Copyright   : (c) Z. K. Edmondson 2018 
License     : GPL-3
Maintainer  : 
Stability   : scratchwork

First steps experimenting with Gloss.
-}


module Vis where

import Control.Monad.Writer (runWriter)

import Graphics.Gloss
import Animate
import qualified Point as Eu 
import qualified Shapes as Eu
import qualified Euclidean as Eu
import Bridge

pictureBack = gradual 0.0 5.0 $ Color (greyN 0.5) $ Pictures $ 
    map toGloss $ 
    snd $ runWriter $ Eu.trefoil (Eu.Radius 40) $ Eu.Circle (Eu.Radius 50) $ Eu.origin 

pictureFore = gradual 5.0 10.0 $ Pictures $ 
    map toGloss $ 
    map Eu.TraceArc $ fst $ runWriter $ Eu.trefoil (Eu.Radius 40) $ Eu.Circle (Eu.Radius 50) $ Eu.origin 

picture x = Pictures $ map ($ x) [pictureFore, pictureBack]

main = animate 
        (InWindow
        "Testing"   -- window title
        (400, 150)      -- window size
        (10, 10))       -- window position
        white                   -- background color
        (picture)                 -- picture to display