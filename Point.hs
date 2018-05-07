{-|
Module      : Point
Description : Primitive 2-D point class
Copyright   : (c) Z. K. Edmondson 2018 
License     : GPL-3
Maintainer  : 
Stability   : experimental

Basic 2-D Point class with some arithmetic operations.
-}

module Point where

data Point = Point {x::Float, y::Float} 
    deriving Show

origin = Point 0 0

(***) :: Float -> Point -> Point
a *** (Point x y) = Point (a*x) (a*y)

(<+>) :: Point -> Point -> Point
a <+> b = Point (x a + x b) (y a + y b)

(<->) :: Point -> Point -> Point
pt1 <-> pt2 = Point (x pt1 - x pt2) (y pt1 - y pt2)

(</>) :: Point -> Float -> Point
(Point x y) </> d = Point (x/d) (y/d)
