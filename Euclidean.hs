module Euclidean where

import Point
import Shapes
import ListHelpers

import Control.Monad.Writer

data TraceShape = TraceCircle Circle | TraceLine Line | TracePoint Point | TraceArc Arc
  deriving Show    

type Tracer = Writer [TraceShape]

circlesIntersect :: Circle -> Circle -> Bool
circlesIntersect (Circle (Radius r1) pt1) (Circle (Radius r2) pt2) =
    let dist = distance pt1 pt2 in
        dist < r1 +r2 
        && dist > abs(r2-r1) 
        && dist /= 0

commonChord :: Circle -> Circle -> Maybe Line
commonChord (Circle (Radius r1) pt1) (Circle (Radius r2) pt2)
    | not $ circlesIntersect(Circle (Radius r1) pt1) (Circle (Radius r2) pt2) = Nothing
    | otherwise = 
    let dist = distance pt1 pt2
        a = (r1*r1 - r2*r2 + dist*dist) / (2 * dist)
        h = sqrt(r1*r1 - a*a)
        pt3 = pt1 <+> ((a *** (pt2 <-> pt1)) </> dist)
        start = Point (x pt3 - h * (y pt2 - y pt1) / dist) (y pt3 + h * (x pt2 - x pt1) / dist) 
        end   = Point (x pt3 + h * (y pt2 - y pt1) / dist) (y pt3 - h * (x pt2 - x pt1) / dist)
        in Just $ Line start end

testChord = commonChord unitCircle (unitCircleAt (Point 1 0))

intersection :: Line -> Line -> Maybe Point
intersection (Line (Point x1 y1) (Point x2 y2)) (Line (Point x3 y3) (Point x4 y4)) =
    let denominator = ((x1 - x2)*(y3 -y4) - (y1 -y2)*(x3 - x4))
        x = ((x1*y2 - y1*x2)*(x3 - x4) - (x1 - x2)*(x3*x4 - y3*y4)) / denominator
        y = ((x1*y2 - y1*x2)*(y3 - y4) - (y1 - y2)*(x3*x4 - y3*y4)) / denominator
        in Just $ Point x y

midpoint :: Line -> Tracer Point
midpoint (Line start end) = 
    let r = Radius (distance start end)
        [circle1, circle2] = map (Circle r) [start, end]
        Just bisector = commonChord circle1 circle2
        Just pt = intersection bisector (Line start end)
    in 
    tell (map TraceCircle [circle1, circle2] ++ [TraceLine bisector]) >> 
    return pt

project :: Point -> Circle -> Point
project pt (Circle (Radius r) center) =
    let dist = distance center pt in
        center <+> (r *** (pt <-> center) </> dist) 

inscribeOnce :: Point -> Circle -> Radius -> Tracer Point
inscribeOnce pt circle r = 
    let result = inscribeOnce' pt circle r in
        tell [TraceCircle (Circle r pt), TraceLine $ Line pt result] >>
        return result
    where inscribeOnce' pt circle r =
            let (Just (Line _ end)) = commonChord circle (Circle r pt)
            in end

inscribe :: Int -> Point -> Circle -> Radius -> Tracer [Point]
inscribe n pt circle radius =
    tell [TraceCircle circle] >>
    iterateNM n (\pt' -> inscribeOnce pt' circle radius) pt 

inscribeHexagon :: Circle -> Tracer [Point]
inscribeHexagon c = inscribe 6 (topMost c) c $ radius c

inscribeEqTriangle :: Circle -> Tracer [Point]
inscribeEqTriangle = liftM takeEvens . inscribeHexagon

makeReuleauxArc :: [Point] -> Arc
makeReuleauxArc (x:y:z:_) = Arc (Circle (Radius $ distance y z) x) y z

reuleaux :: Circle -> Tracer [Arc]
reuleaux =  liftM (take 3 . map makeReuleauxArc . rotations) . inscribeEqTriangle 

makeArc :: Radius -> [Point] -> Arc
makeArc r (x:y:z:_) = Arc (Circle r x) y z

trefoilLobe :: Circle -> Circle -> Arc
trefoilLobe base lobe =
    let (Just (Line s e)) = commonChord lobe base in
        Arc lobe s e

trefoil :: Radius -> Circle -> Tracer [Arc]
trefoil (Radius r) c = liftM (map $ trefoilLobe c . Circle (Radius r)) $ inscribeEqTriangle c 