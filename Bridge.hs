{-|
Module      : Bridge
Description : Conversion from my shape types to Gloss.Picture 
Copyright   : (c) Z. K. Edmondson 2018 
License     : GPL-3
Maintainer  : 
Stability   : experimental

Original Euclidean code didn't follow any pre-existing 
shape types, so this file provides converters.
-}

module Bridge where

import Point
import Shapes
import Euclidean
import qualified Graphics.Gloss as Gl

toGloss :: TraceShape -> Gl.Picture
toGloss (TracePoint _) = Gl.Blank
toGloss (TraceLine (Line (Point x y) (Point x1 y1))) = 
    Gl.Line [(x,y), (x1,y1)]
toGloss (TraceCircle (Circle (Radius r) (Point x y))) = 
    Gl.Translate x y $ Gl.Circle r
toGloss (TraceArc (Arc (Circle (Radius r) (Point x y)) (Point x1 y1) (Point x2 y2))) =
    Gl.Translate x y $ Gl.Arc (180/pi*flip atan2 (x1 - x) (y1 - y)) (180/pi*flip atan2 (x2 - x) (y2 - y)) r