{-|
Module      : ListHelpers
Description : Convenience functions that I couldn't find anywhere else
Copyright   : (c) Z. K. Edmondson 2018 
License     : GPL-3
Maintainer  : 
Stability   : experimental

Some list functions that I couldn't find in the standard libraries.
-}

module ListHelpers where

rotate :: [a] -> [a]
rotate (x:xs) = xs ++ [x]
    
rotations :: [a] -> [[a]]
rotations = iterate rotate    

takeEvens :: [a] -> [a]
takeEvens (x:y:xs) = x:takeEvens xs
takeEvens [x] = [x]
takeEvens [] = []

takeOdds :: [a] -> [a]
takeOdds (x:xs) = takeEvens xs
takeOdds [] = []

iterateNM :: Monad m => Int -> (a -> m a) -> a -> m [a]
iterateNM n f x = iterateNM' n f x []
    where iterateNM' n f x ys
            | n == 0    = return ys
            | otherwise = (f x) >>= \y -> iterateNM' (n-1) f y (y:ys)  
