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
import qualified Point as Eu 
import qualified Shapes as Eu
import qualified Euclidean as Eu
import Bridge

pictureBack = Color (greyN 0.5) $ Pictures $ 
    map toGloss $ 
    snd $ runWriter $ Eu.reuleaux $ Eu.Circle (Eu.Radius 50) $ Eu.origin 

pictureFore = Pictures $ 
    map toGloss $ 
    map Eu.TraceArc $ fst $ runWriter $ Eu.reuleaux $ Eu.Circle (Eu.Radius 50) $ Eu.origin 


main = display 
        (InWindow
        "Testing"   -- window title
        (400, 150)      -- window size
        (10, 10))       -- window position
        white                   -- background color
        (Pictures [pictureBack, pictureFore])                 -- picture to display

--picture=Translate (-170) (-20)    -- shift the text to the middle of the window
--    $ Scale 0.5 0.5             -- display it half the original size
--    $ Text "Hello World"    -- text to display