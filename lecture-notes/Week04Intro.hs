module Week04Intro where

import Prelude hiding (foldr, foldl)

-- ANNOUNCEMENTS:

----  Coursework released
----    - See MyPlace for details
----    - Deadline is Monday of Week 11 (29th November)
----    - Get started by clicking the 'Fork' button on GitLab
----    - Note the marking scheme; it is worth 50% of the mark for CS316

---- Class test
----    - Monday 26th - Tuesday 27th
----    - 10 short questions, similar to the tutorial ones
----    - Also counts for 50%




{- PATTERNS OF RECURSION

   When writing Haskell programs, or programs in any language, we often see
   repeated patterns.

   So it often a good idea to examine those patterns and turn them into
   reusable pieces of code.

   "Abstract out" the common patterns of recursion in Haskell programs.

   Crucial tool for this is Higher Order Functions.
-}



{- foldr -}

len :: [a] -> Int 
len []     = 0
len (x:xs) = 1 + len xs

total :: [Int] -> Int
total []     = 0
total (x:xs) = x + total xs

allTrue :: [Bool] -> Bool
allTrue []     = True -- but is this right? why?
allTrue (x:xs) = x && allTrue xs

--   allTrue [True]
-- = True && allTrue []
-- = True && True         -- if allTrue [] = False, then this case is True && False = False
-- = True

anyTrue :: [Bool] -> Bool 
anyTrue []     = False
anyTrue (x:xs) = x || anyTrue xs

-- "population count"
popCount :: Eq a => a -> [a] -> Int 
popCount x []     = 0
popCount x (y:ys) = (if x == y then 1 else 0) + popCount x ys
--   case y of
--       x -> 1 + popCount x ys -- this does not mean 'is equal to x'
--       _ -> popCount x ys


-- What's the common pattern?

-- In each of these functions, we have two cases:
--   1. What to do with the empty list '[]'.
--   2. What to do with the list with at least one thing in.
--           i.e. a list with a head and a tail
--
-- Why this split of cases? Come back to this later.

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f b []     = b 
foldr f b (x:xs) = f x (foldr f b xs)

-- we want  ??? :: b
-- we have b     :: b
--         x     :: a
--         xs    :: [a]
--         f     :: a -> b -> b
--         foldr :: (a -> b -> b) -> b -> [a] -> b

len_v2 :: [a] -> Int
len_v2 = foldr (\a l -> 1+l) 0

--   len_v2 [2,3,4,5,7]
-- = foldr (\a l -> 1+l) 0 [2,3,4,5,7]
-- = (\a l -> 1+l) 2 (foldr (\a l -> l+1) 0 [3,4,5,7])
-- = 1 + (foldr (\a l -> l+1) 0 [3,4,5,7])
-- = 1 + ((\a l -> l+1) 3 (foldr (\a l -> 1+l) 0 [4,5,7]))
-- = 1 + (1 + (foldr (\a l -> 1+l) 0 [4,5,7]))
-- = 1 + (1 + (1 + (1 + (1 + (foldr (\a l -> 1+l) 0 [])))))
-- = 1 + (1 + (1 + (1 + (1 + 0))))
-- = 5

total_v2 :: [Int] -> Int 
total_v2 = foldr step base
  where base = 0
        step x t = x + t
  -- foldr (\x t -> x + t) 0

-- Exercise: write out allTrue and anyTrue and popCount using foldr.



{- fold for other datatypes -}

-- Why is 'foldr' like that?
--
-- Answer is because that is lists are defined:
--
--  data [a] = [] | (:) a [a]
--
-- Two constructors, a list is either:
-- -- the empty list '[]'
-- -- or a head and a tail 'x : xs'

-- Consequently, we have two cases in foldr.



data BoolExpr
    = Atom String
    | And  BoolExpr BoolExpr
    | Or   BoolExpr BoolExpr
    | Not  BoolExpr
    deriving Show

foldBoolExpr :: (String -> b) -- ^ for atoms
             -> (b -> b -> b) -- ^ for And
             -> (b -> b -> b) -- ^ for Or
             -> (b -> b)      -- ^ for Not
             -> BoolExpr
             -> b
foldBoolExpr atom and or not (Atom x)  = atom x
foldBoolExpr atom and or not (And p q) = and (foldBoolExpr atom and or not p) (foldBoolExpr atom and or not q)
foldBoolExpr atom and or not (Or p q)  = or (foldBoolExpr atom and or not p) (foldBoolExpr atom and or not q)
foldBoolExpr atom and or not (Not p)   = not (foldBoolExpr atom and or not p)

-- evaluate a boolean expression to a boolean, under the assumption that all atoms are 'True'
eval_v1 :: BoolExpr -> Bool
eval_v1 = foldBoolExpr (\_ -> True) -- "A function '\' that takes an argument we don't care about '_' and returns 'True'"
                       (&&)
                       (||)
                       not

-- This pattern is common across many programming languages.
-- In OO (Object Oriented) languages like Java, it is commonly called the 'Visitor' pattern,
-- because we are "visiting" each constructor in the input.




{- List comprehensions -}

-- List comprehensions:
--
--   [ (x,y) | x <- [1,2,3,4], y <- [5,6,7,8] ]
-- Watch the videos to learn more about list comprehensions.
--
-- List comprehensions are also available in Python.



