module Animate where

import Graphics.Gloss

gradualFrac :: Float -> Picture -> Picture
gradualFrac frac p 
    | frac < 0.0 = Blank
    | frac > 1.0 = p
    | otherwise = gradualFrac' frac p 
        where gradualFrac' frac (Arc a b r) = Arc a (a + frac * (b - a)) r
              gradualFrac' frac (Circle r) = gradualFrac' frac $ Arc 0 360 r
              gradualFrac' frac (Line [(x1,y1), (x2, y2)]) = Line [(x1, y1), (x1 + frac * (x2 - x1), y1 + frac * (y2 - y1))]  

gradual :: Float -> Float -> Picture -> Float -> Picture
gradual tStart tFinal (Circle r) t = gradualFrac ((t - tStart) / tFinal) $ Circle r 
gradual tStart tFinal (Translate x y p) t = Translate x y $ gradual tStart tFinal p t
gradual tStart tFinal (Color c p) t = Color c $ gradual tStart tFinal p t
gradual tStart tFinal (Pictures ps) t = Pictures $ map (\p -> gradual tStart tFinal p t) ps  
gradual tStart tFinal (Line ls) t = gradualFrac ((t - tStart) / tFinal) $ Line ls
gradual tStart tFinal (Arc a b r) t = gradualFrac ((t - tStart) / tFinal) $ Arc a b r