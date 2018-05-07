{-|
Module      : Shapes
Description : Primitive shapes 
Copyright   : (c) Z. K. Edmondson 2018 
License     : GPL-3
Maintainer  : 
Stability   : experimental

Primitive shape objects build up from Points.
-}

module Shapes where 

import Point

-- | The Segment type class is for 
-- types with a definitive start and end
-- points.
class Segment a where
    start :: a -> Point
    end :: a -> Point

-- | The Line data type represents
-- a line segment from start to end.
data Line = Line Point Point
    deriving Show

-- | Line is the most primitive instance
-- of Segment.
instance Segment Line where
    start (Line a _) = a
    end (Line _ a) = a

-- | Not sure whether Radius
-- really deserves its own 
-- data definition, but I gave it one. 
data Radius = Radius Float
    deriving Show

-- | A circle is just a center
-- with a radius.  
data Circle = Circle {radius::Radius, center::Point}
    deriving Show

-- | 
data Arc = Arc Circle Point Point  
    deriving Show

instance Segment Arc where
    start (Arc _ a _) = a
    end   (Arc _ _ a) = a

unitCircleAt :: Point -> Circle
unitCircleAt pt = Circle (Radius 1) pt
unitCircle = unitCircleAt origin

leftMost :: Circle -> Point
leftMost (Circle (Radius r) (Point x y)) = Point (x - r) y

topMost  :: Circle -> Point
topMost (Circle (Radius r) (Point x y)) = Point x (y + r)

reposition :: Circle -> Point -> Circle
reposition (Circle r _) pt = Circle r pt

distance :: Point -> Point -> Float
distance (Point x1 y1) (Point x2 y2) =
    let square x = x*x in
        sqrt(square (x2 - x1) + square (y2 - y1))  
