{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week07Intro where

import Prelude hiding ( Monad (..)
                      , Applicative (..)
                      , mapM
                      , mapM_
                      , (<$>))
import Data.Char      (isDigit, digitToInt)

{- This is needed due to the RebindableSyntax extension. I'm using this
   extension so the 'do' notation in this file uses my redefined
   'Monad' type class, not the standard library one. RebindableSyntax
   lets the user redefine what 'do', and 'if' mean. I've given 'if'
   the standard meaning here: -}
ifThenElse True  x y = x
ifThenElse False x y = y
(>>) x y = x >>= \_ -> y


{------------------------------------------------------------------------}
{-   WEEK 7 : MONADS                                                    -}


{- Week 06 Class Test:  50%
   - if you got >= 40, then you've passed the course
   - if you got < 40, then I strongly recommend that you take the Week 09 resit test

   Final mark = max(week06,week09) + coursework
-}


{- 1. DEFINING MONADS and THE MAYBE MONAD -}

{- Last week:

   - Simulating side effects:
     - How to simulate exceptions using 'Maybe'
     - How to simulate state using state passing style
     - How to simulate printing by keeping a list of things we have printed

  - They all have a common core
    - They all have "do nothing command":
      - returnOk           :: a -> Maybe a
      - return             :: a -> State a
      - returnWithPrinting :: a -> Printing a
      - End                :: a -> Process a

    - They all have a "do this, then do that" function:
      - ifOk                :: Maybe a    -> (a -> Maybe b)    -> Maybe b
      - andThen             :: State a    -> (a -> State b)    -> State b
      - andThenWithPrinting :: Printing a -> (a -> Printing b) -> Printing b
      - sequ                :: Process a  -> (a -> Process b)  -> Process b

  - We've got four examples, and (I promise) there are many many more.

  - So it is worth capturing this pattern in a typeclass, so that:
    (a) we give it a name so that we can talk about it
    (b) we can write functions that work for all members of this typeclass

  - Due to historical reasons, the name given is 'Monad'

  - This is not a great name. It comes from a field of mathematics called "Category Theory"

  - All we need to know is that it represents types that have:
    (a) a do nothing operation
    (b) a do this and then do that operation

  - In the notes: there is a link to talk by Simon Peyton Jones from 2004, a retrospective of the first 15 years of Haskell
    - in it, he claims that Monads should have been called "Warm Fuzzy Things"
-}

class Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b  -- said out loud "bind"

instance Monad Maybe where
  -- this is exactly the same as the 'returnOk' function
  return a = Just a

  -- this is exactly the same as the 'ifOK' function
  Nothing >>= f = Nothing
  Just a  >>= f = f a


search :: Eq k => k -> [(k,v)] -> Maybe v
search k [] = Nothing
search k ((k',v'):kvs) =
  if k == k' then
    return v'
  else
    search k kvs

lookupAll :: Eq k => [(k,v)] -> [k] -> Maybe [v]
lookupAll kvs [] = return []
lookupAll kvs (k:ks) =
  search k kvs     >>= \v ->
  lookupAll kvs ks >>= \vs ->
  return (v:vs)

{- This is a bit nicer than the previous one (from Week 06), but really
   the only difference so far is that we have replaced 'ifOK' with '>>='.
-}

{- 2. 'do' NOTATION -}

{- Haskell defines syntactic sugar for writing functions that use Monads. -}

lookupAll_v2 :: Eq k => [(k,v)] -> [k] -> Maybe [v]
lookupAll_v2 kvs [] = return []
lookupAll_v2 kvs (k:ks) =
  do v  <- search k kvs
     vs <- lookupAll_v2 kvs ks
     return (v:vs)


{- 3. FUNCTIONS FOR ALL MONADS -}

-- map :: (a -> b) -> [a] -> [b]

-- What we would like to write, is something like:

lookupAll_v3 :: Eq k => [(k,v)] -> [k] -> Maybe [v]
lookupAll_v3 kvs = mapM (\k -> search k kvs)

mapMaybe :: (a -> Maybe b) -> [a] -> Maybe [b]
mapMaybe f []     = return []
mapMaybe f (a:as) =
  do b  <- f a
     bs <- mapMaybe f as
     return (b:bs)

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f []     = return []
mapM f (a:as) =
  do b  <- f a
     bs <- mapM f as
     return (b:bs)

forM :: Monad m => [a] -> (a -> m b) -> m [b]
forM xs f = mapM f xs

lookupAll_v4 :: Eq k => [(k,v)] -> [k] -> Maybe [v]
lookupAll_v4 keyValueStore ks =
  forM ks $ \k ->
    search k keyValueStore
