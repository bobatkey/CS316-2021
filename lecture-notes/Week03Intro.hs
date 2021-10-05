module Week03Intro where

import Prelude hiding (filter, map)

{- Week 03 : Higher order functions

   Key idea: functions are values that can be passed around
   as arguments, returned from functions, and stored in data
   structures.
-}

add :: Int -> (Int -> Int)
    -- Int -> Int -> Int   -- means exactly the same thing as the line above
add x y = x + y

-- 2 + 3 + 4 is the same as 2 + (3 + 4)

-- Partial application:
add2 :: Int -> Int
add2 = add 2
-- because we are partially applying 'add'.

--   add2 10
-- = add 2 10
-- = 2 + 10
-- = 12

-- "Lambda" notation

add' :: Int -> (Int -> Int)
add' =  \x  -> (\y  -> x + y)

-- Partial application of add':
--   add' 2
-- = (\x -> (\y -> x + y)) 2
-- = (\y -> 2 + y)

add'' :: Int -> (Int -> Int)
add'' x = \y -> x + y

-- λ-calculus: λx. λy. x+y
-- Haskell:    \x -> \y -> x + y
-- Javascript: function (x) { return function (y) { return x + y; } }
-- Javascript: x => y => x + y
-- Java:       x -> y -> x + y
-- Python:     lambda x: lambda y: x+y

-- "Anonymous functions"
--    because we needn't name everything.
-- a programming joke:
--   there are only two hard things in computer science,
--   naming things, cache invalidation and off by one errors.

-- "Practical example" of partial application

makeParagraph :: String
makeParagraph = "<p>My <strong>text</strong></p>"

{-}
p :: String -> String
p text = "<p>" ++ text ++ "</p>"

strong :: String -> String
strong text = "<strong>" ++ text ++ "</strong>"
-}

makeParagraph' :: String
makeParagraph' = p ("My " ++ strong "text")

tag :: String -> String -> String
tag elementName body = "<" ++ elementName ++ ">" ++ body ++ "</" ++ elementName ++ ">"

p :: String -> String
p = tag "p"

strong :: String -> String
strong = tag "strong"

h1 :: String -> String
h1 = tag "h1"

--    p "body"
-- == tag "p" "body"
-- == "<" ++ "p" ++ ">" ++ "body" ++ "</" ++ "p" ++ ">"
-- == "<p>body</p>"

---------------------------------------------
-- Functions that take functions as arguments
---------------------------------------------

popCount :: Int -> [Int] -> Int
popCount x []     = 0
popCount x (y:ys) = (if (==) x y then 1 else 0) + popCount x ys

popCount' :: (Int -> Bool) -> [Int] -> Int
popCount' predicate []     = 0
popCount' predicate (y:ys) = (if predicate y then 1 else 0) + popCount' predicate ys

filter :: (a -> Bool) -> [a] -> [a]
filter predicate [] = []
filter predicate (x:xs) =
    if predicate x then x : filter predicate xs
                   else filter predicate xs

db :: [(String,Int)]
db = [ ("Ben Nevis",     1300)
     , ("Mont Blanc",    3000)
     , ("Mount Everest", 6000)
     , ("Snowden",        500)
     , ("Fuji",          2500)
     ]

mountainsOver2000withAShortName :: [(String,Int)]
mountainsOver2000withAShortName = filter myPredicate db
  where myPredicate (name, height) = height >= 2000 && length name < 5
     -- myPredicate = \(name, height) -> height >= 2000 && length name < 5

justTheNames :: [(String,Int)] -> [String]
justTheNames []                       = []
justTheNames ((name, height):records) = name : justTheNames records

map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs

ourQuery :: [String]
ourQuery =
    map (\(name, height) -> name ++ " has height " ++ show height)
        (filter (\(name, height) -> height >= 2000)
                db)

                