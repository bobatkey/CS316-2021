module Week09Intro where

import Week08

{- Week 09 : DATA DEPENDENCIES and APPLICATIVE FUNCTORS -}

-- To get parallelism (running multiple things on separate hardware)
-- we need to avoid data dependencies.

-- In Haskell, we have exposed the sequentialness of normal
-- programming. In the Monad interface, we have the basic operation of
-- "do this, then do that":
--
--    (>>=) :: m a -> (a -> m b) -> m b
--             (i)    (ii)          (iii)
-- (>>=) "bind" takes (i) an operation that returns an 'a'
--                    (ii) an continutation that needs an 'a', and returns an operation that will return a 'b'
--            returns (iii) a complete operation doing the two, that returns a 'b'
--
-- The Monad interface "builds in" a data dependency between the two
-- operations.

mapM_v2 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM_v2 f []     = return []
mapM_v2 f (x:xs) = do y  <- f x
                      ys <- mapM_v2 f xs
                      return (y:ys)

-- "Applicative Functors"
--   -- 2008. Ross Paterson and Conor McBride

-- (<*>) :: m (a -> b) -> m a -> m b
-- pure  :: a -> m a

mapM_v3 :: Applicative m => (a -> m b) -> [a] -> m [b]
mapM_v3 f []     = pure []
mapM_v3 f (x:xs) = pure (:) <*> f x <*> mapM_v3 f xs

-- map :: (a -> b) -> [a] -> [b]
-- map f []     = []
-- map f (x:xs) = (:) (f x) (map f xs)

-- 1. Every Monad is an Applicative Functor
-- 2. But we can also choose to have a different implementation of (<*>)

-- As a larger example:
--
-- Facebook's Haxl library uses a special applicative functor called
-- 'Fetch' that automatically parallelises requests to backend servers
-- for their spam filtering.
--
-- This speeds up the process of querying many databases and other
-- services within Facebook for the purpose of filtering spam.

-- In videos and lecture notes:
--    1 + 2 : An introduction to Applicative Functors
--    3     : Concurrent programming in Haskell (forkIO, MVars)
--    4     : An example of how to write a concurrent object for Logging
--    5     : A toy version of the Facebook library that executes HTTP requests in parallel













{- Part I : Sequences of Actions -}













{- Part II : Applicative Functors -}












{- Part III : Pictures -}




{- Part IV : Data Dependencies and Parallelism -}
