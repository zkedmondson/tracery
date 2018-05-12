{-|
Module      : Counter
Description : Counter monad 
Copyright   : (c) Z. K. Edmondson 2018 
License     : GPL-3
Maintainer  : 
Stability   : experimental

Counter Monad, mainly so I can re-learn monads and avoid 
overkill of using the State monad for this purpose.

-}

module Counter where

import Control.Applicative
import Control.Monad

newtype Counter a = Counter {runCounter::(a, Int)} 
    deriving (Show)

instance Functor Counter where
    fmap f = \(Counter (x,i)) -> Counter (f x, i) 

instance Applicative Counter where
    pure = \x -> Counter (x, 0)
    liftA2 f = \(Counter (x,i)) -> (\(Counter (y,j)) -> Counter (f x y, i+j))

instance Monad Counter where
    (Counter (x,c)) >>= f = let (Counter (y,c')) = f x in Counter (y,(c+c'))
    return x = Counter (x,0) 

increment :: Counter ()
increment = Counter ((), 1)

