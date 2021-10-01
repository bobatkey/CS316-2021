{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week02Solutions where

import Week02

{------------------------------------------------------------------------------}
{- TUTORIAL QUESTIONS                                                         -}
{------------------------------------------------------------------------------}

{- In the questions below, replace 'undefined' with your answers. Use
   GHCi to test them.-}

{- 1. Write a function that counts the number of occurrences of an
      element in list: -}

-- Three alternate solutions:

popCount1 :: Eq a => a -> [a] -> Int
popCount1 y []     = 0
popCount1 y (x:xs) = (if x == y then 1 else 0) + popCount1 y xs

popCount2 :: Eq a => a -> [a] -> Int
popCount2 y [] = 0
popCount2 y (x:xs)
   | x == y    = 1 + popCount2 y xs
   | otherwise = popCount2 y xs

popCount3 :: Eq a => a -> [a] -> Int
popCount3 y []     = 0
popCount3 y (x:xs) = if x == y then 1 + popCount3 y xs else popCount3 y xs

{-    (popCount is short for "population count"). Examples:

         popCount 2 [1,2,5,2,7,2,9] == 3
         popCount 9 [1,2,5,2,7,2,9] == 1
         popCount 0 [1,2,5,2,7,2,9] == 0
-}


{- 2. Write a version of 'insert' that only inserts into a sorted list
      if the element is not already there. Examples:

         insertNoDup 2 [1,3,4]   == [1,2,3,4]
         insertNoDup 2 [1,2,3,4] == [1,2,3,4]
-}

-- Four possible solutions:

-- Using a guard, similar to the 'insert' above, but with an
-- additional check.
insertNoDup :: Ord a => a -> [a] -> [a]
insertNoDup x [] = [x]
insertNoDup x (y:ys)
  | x < y     = x : y : ys
  | x == y    = y : ys
  | otherwise = y : insertNoDup x ys

-- Doing a pre-check and then inserting if it isn't there: does two
-- scans of the list, instead of one.
insertNoDup2 :: Ord a => a -> [a] -> [a]
insertNoDup2 y xs = if popCount1 y xs == 0 then insert y xs else xs

-- Doing a pre-check and then sorting the new element in. Takes n^2
-- time in the worst case.
insertNoDup3 :: Ord a => a -> [a] -> [a]
insertNoDup3 a [] = [a]
insertNoDup3 a (x:xs) = if popCount1 a (x:xs) <= 0 then isort (a:x:xs) else (x:xs)

-- Variant of the first solution, but using a case expression instead
-- of guards.
insertNoDup4 :: Ord a => a -> [a] -> [a]
insertNoDup4 y [] = [y]
insertNoDup4 y (x:xs) = case compare y x of
                          LT -> y:x:xs
                          EQ -> x:xs
                          GT -> x:insertNoDup4 y xs

{- 3. Write a version of 'remove' that removes all copies of an element
      from a sorted list, not just the first one. Examples:

         removeAll 2 [1,2,2,3] == [1,3]
         removeAll 2 [1,3]     == [1,3]
-}

-- Similar to editing 'insert' to get 'insertNoDup', can edit 'remove'
-- to get 'removeAll'.
removeAll :: Ord a => a -> [a] -> [a]
removeAll y [] = []
removeAll y (x:xs)
  | x == y    = removeAll y xs
  | x < y     = x:removeAll y xs
  | otherwise = x:xs


{- 4. Rewrite 'treeFind' and 'treeInsert' to use 'compare' and 'case'
      expressions. -}

treeFind2 :: Ord k => k -> KV k v -> Maybe v
treeFind2 k Leaf = Nothing
treeFind2 k (Node l (k',v') r) =
  case compare k k' of
    LT -> treeFind2 k l
    EQ -> Just v'
    GT -> treeFind2 k r


-- An example tree: Node (Node (Node Leaf ("A",0) Leaf) ("a",1) Leaf) ("b",2) (Node Leaf ("c",3) Leaf)
{-             ("b",2)
            /          \
        ("a",1)         ("c",3)
    ("A",0)      Leaf   Leaf      Leaf
 Leaf    Leaf
-}

treeInsert2 :: Ord k => k -> v -> KV k v -> KV k v
treeInsert2 k v Leaf = Node Leaf (k,v) Leaf
treeInsert2 k v (Node l (k',v') r) =
  case compare k k' of
    EQ -> Node l (k,v) r
    LT -> Node (treeInsert2 k v l) (k',v') r
    GT -> Node l (k',v') (treeInsert2 k v r)

{- 5. MergeSort is another sorting algorithm that works in the following
      way:

      - If the list to be sorted is zero length, then it is already
        sorted.

      - If the list to be sorted has one element, then it is already
        sorted.

      - Otherwise, split the list into two, one with the even elements
        and one with the odd elements. Sort the two lists by calling
        'mergeSort' recursively. Then merge the two lists together
        maintaining the ordering.

      Write this function in three parts: -}

{-    'split' splits the input into two lists: one with the odd numbered
      elements and one with the even numbered elements. HINT: you can
      pattern match on multiple elements at the head of a list with
      'x1:x2:xs', and you can use the '(odds,evens) = ...' syntax in a
      'where' clause. -}

-- Three possible solutions:

-- Pattern matching on two elements at a time
split :: [a] -> ([a], [a])
split []         = ([],[])
split (x:[])     = ([x],[])
split (x1:x2:xs) = (x1:odds, x2:evens)
  where (odds, evens) = split xs

-- Pattern matching on one element at a time, switching the meaning of
-- odd and even elements:
split2 :: [a] -> ([a],[a])
split2 [] = ([], [])
split2 (x:xs) = (x:evens, odds)
  where (odds, evens) = split2 xs

-- A solution using list comprehensions (See Week 04)
split3 :: [a] -> ([a], [a])
split3 xs = (odds,evens)
  where odds  = [x | (x,y) <- zip xs [1..], odd y]
        evens = [x | (x,y) <- zip xs [1..], even y]

{-    'merge' merges two sorted lists into one sorted list. Examples:

          merge [1,3,5] [2,4,6]  = [1,2,3,4,5,6]
          merge [1,3,5] [7,9,11] = [1,3,5,7,9,11]
-}

-- merging works as follows:
merge :: Ord a => [a] -> [a] -> [a]
merge []     ys     = ys      -- if there is only one list to merge...
merge xs     []     = xs      -- ... then just return it
merge (x:xs) (y:ys)           -- otherwise, take one element from both
  | x <= y    = x : merge xs (y:ys)   -- if 'x' is lower, output it and merge 'xs' and 'y:ys'
  | otherwise = y : merge (x:xs) ys   -- if 'y' is lower, do the symmetric thing.


{-    'mergeSort' uses 'split' and 'merge' to implement the merge sort
      algorithm described above. -}

mergeSort :: Ord a => [a] -> [a]
mergeSort []  = []  -- empty list is sorted
mergeSort [x] = [x] -- one element list is sorted
mergeSort xs  = merge (mergeSort xs1) (mergeSort xs2)
  where (xs1,xs2) = split xs
     -- otherwise, split, sort recursively and then merge.


{- 6. Write another version of 'makeChange' that returns all the
      possible ways of making change as a list: -}

-- The following solution is a roundabout way of getting to the
-- solution to demonstrate the similarity between the 'Maybe' version
-- and the list version. You're not expected to have come up with this
-- chain of thought.

-- here is the original 'makeChange', renamed to 'makeChange1':
makeChange1 :: [Coin] -> [Coin] -> Int -> Maybe [Coin]
makeChange1 coins        used 0 = Just used
makeChange1 []           used _ = Nothing
makeChange1 (coin:coins) used amount
  | amount >= coin =
    case makeChange1 coins (coin:used) (amount - coin) of
      Just coins -> Just coins
      Nothing    -> makeChange1 coins used amount
  | otherwise =
    makeChange1 coins used amount

-- We can think of 'Maybe' as a kind of container that can contain
-- zero or one elements. Similarly, we can think of lists as
-- containers that contain zero, one, two, three, ... elements.

-- Thinking like this, we can think: what does it mean to "append" two
-- 'Maybe's? One answer is:

addMaybe :: Maybe a -> Maybe a -> Maybe a
addMaybe Nothing  y = y
addMaybe (Just x) _ = Just x

-- Which is like list append (Week 01), except that if the first
-- 'Maybe' contains something, then we ignore the second one.
--
-- With this function we can rewrite 'makeChange' to have the same
-- behaviour:

makeChange2 :: [Coin] -> [Coin] -> Int -> Maybe [Coin]
makeChange2 coins        used 0 = Just used
makeChange2 []           used _ = Nothing
makeChange2 (coin:coins) used amount
  | amount >= coin =
    makeChange2 coins (coin:used) (amount - coin)
    `addMaybe`
    makeChange2 coins used amount
  | otherwise =
    makeChange2 coins used amount

-- Now we can convert to lists instead of 'Maybe':
--
-- 1. 'Just used' becomes '[used]'   (a container with one element)
-- 2. 'Nothing'   becomes '[]'       (a container with no elements)
-- 3. 'addMaybe'  becomes '++'       (different way of adding containers)

--So we get:

makeChangeAll :: [Coin] -> [Coin] -> Int -> [[Coin]]
makeChangeAll coins        used 0 = [used]
makeChangeAll []           used _ = []
makeChangeAll (coin:coins) used amount
  | amount >= coin =
    makeChangeAll coins (coin:used) (amount - coin)
    ++
    makeChangeAll coins used amount
  | otherwise =
    makeChangeAll coins used amount

-- Now we can ask for all ways to make change:
--
--   > makeChangeAll [50,20,20,10,2,2,1] [] 54
--   [[2,2,50],[2,2,10,20,20]]

{- HINT: you don't need a case expression, just a way of appending two
   lists of possibilities. -}
