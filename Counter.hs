{-|
Module      : Counter
Description : Counter monad 
Copyright   : (c) Z. K. Edmondson 2018 
License     : GPL-3
Maintainer  : 
Stability   : experimental

Counter Monad, mainly so I can re-learn monads and avoid 
overkill of using the State monad for this purpose.  Note
that this monad is basically the State monad where the state
is restricted to Int and the accessor/mutator functions
are increment and current

-}

module Counter where

import Control.Applicative
import Control.Monad

newtype Counter a = Counter {runCounter::Int -> (a, Int)} 
--    deriving (Show)

instance Functor Counter where
    fmap f (Counter g) = 
        Counter $ \i -> 
            let (a, i1) = g i
            in (f a, i1)

instance Applicative Counter where
    pure x = Counter $ \i -> (x,i)
    liftA2 f (Counter g) (Counter h) = 
        Counter $ \i -> 
            let (a,i) = g i
                (b,j) = h i
            in (f a b, i+j)   

instance Monad Counter where
    return x = Counter $ \i -> (x, i)
    (Counter g) >>= f = 
        Counter $ \s -> let (a, i') = g s
                            (Counter h) = f a
                         in h i'

increment :: Counter ()
increment = Counter $ \i -> ((), i+1)

current :: Counter Int
current = Counter $ \i -> (i,i)

testCounter = do
    increment 
    increment
    a <- current
    increment
    increment
    return a