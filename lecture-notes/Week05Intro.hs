module Week05Intro where

import Prelude hiding (Left, Right, Semigroup (..), Foldable (..), Functor (..), Monoid (..), Maybe (..))
import Data.Char (toUpper)
import GHC.Generics (V1)

-- 1. Remember there is the class test next Monday
-- 2. The coursework has been released, and I recommend that you start sooner rather than later
-- 3. Please fill in the mid-semester course feedback form

{- Week 05 : Classes of Types

-}

-- Type design

data PlayingCardSuit
    = Hearts
    | Clubs
    | Diamonds
    | Spades
    deriving (Show, Eq)

-- enum PlayingCardSuit { HEARTS, CLUBS, DIAMONDS, SPADES }

{- A student record consists of:
   - Student name (String)
   - One, or both of registration number and DS username (abc208768)
-}

{- In Java:
   class StudentRecord {
       // this is never null
       @Nonnull
       public final String studentName;

       // at least one of these is not null
       public final String registrationNumber;
       public final String dsUsername;
   }
-}

data StudentIDDetails
    = RegNumber  String 
    | DSUsername String
    | BothRegAndDS String String
    deriving (Show, Eq)

data StudentRecord =
    MkStudentRecord { studentName    :: String
                    , studentDetails :: StudentIDDetails
                    }
    deriving (Show, Eq)

-- Example of a valid student record:

aStudent :: StudentRecord
aStudent = MkStudentRecord { studentName = "A Student", studentDetails = DSUsername "abc2072346" }

-- There is no way to make a StudentRecord that doesn't have at least one of a registration number
-- or DS username.
--
-- The key to using types in Haskell is to make it impossible to say the wrong thing.
--
-- "Make Illegal States Unrepresentable"
--
-- Orwell's 1984: Newspeak -- make a language is which bad thoughts can not be expressed.

-- "Stringly typed programming"  (by parody of "Strongly Typed")
-- which using strings as a universal type for everything.

-- Haskell has no concept of 'null'.
-- and consequently, no concept of NullPointerException.

-- 'null' is ambiguous in terms of what it means:
--
--   HashMaps have a .get(K key) method which returns the value associated to that key
--   - if the value is 'null', then it returns null
--   - if the key is not there, it returns null

-- In Haskell: we have
--    - Just Nothing    <-- if the value is Nothing
--    - Nothing         <-- if the key is not present

lookup :: Eq k => k -> [(k,v)] -> Maybe v
lookup = undefined 

data Maybe a
  = Nothing
  | Just a
  deriving (Eq, Show)

-- C. A. R. Hoare, inventor of Quicksort, and the null pointer.
--
--   "Billion dollar mistake"
--
-- due to amount of time spent debugging NullPointerExceptions (and segfaults, etc.)

-- In Java, there is now a class Optional<V> which is a Java version of Maybe.




------------------------------------------
-- Type classes

-- Haskell's method for 'overloading', which is using the same name or symbol to mean different things
-- for different types.
--
-- Most languages:  (==) is overloaded, as are (+), (*), (-)
--
-- In Java: multiple methods can have the same name, but different types
--          interfaces describe a set of methods that a class may provide
--
-- In Haskell: we have "Type classes", which are classes of types which support common operations

-- The 'Show' typeclass:
--   If a type 'a' is in the 'Show' typeclass, then there is a function 'show :: a -> String'

-- The 'Eq' typeclass:
--   If a type 'a' is in the 'Eq' typeclass, then there is a function '(==) :: a -> a -> Bool'

-- So far, we have be writing:
--    deriving (Eq, Show)
-- which tells the compiler to write for us a default implementation.

-- But we don't have to use the compiler's default implementations.

data CISString = MkCIS String

instance Show CISString where
    show (MkCIS string) = show (map toUpper string)

instance Eq CISString where
    MkCIS str1 == MkCIS str2 = (map toUpper str1) == (map toUpper str2)

-- Show -- 'show' function
-- Eq   -- (==) function
-- Ord  -- compare function, which gives (<), (>), (<=), (>=)
-- Num  -- numeric functions


----------------------------------------------------------
-- Semigroups and Monoids

-- The basic idea is that we want to capture (or "express") what it means
-- to be able to combine two things into one.
--
-- Why? Because this is the essence of "summarising" or "aggregating" some
-- collection of values.

-- A semigroup is:
--   1. a collection (mathematically, a set) of things. In Haskell, a type
--   2. an operation (<>) that combines two things

class Semigroup a {- the collection of things -} where
    (<>) :: a -> a -> a {- the combining function -}

instance Semigroup Int where
    x <> y = x + y

instance Semigroup [a] where
    x <> y = x ++ y

instance Semigroup Bool where
    x <> y = x && y

-- Left-biased combination
instance Semigroup (Maybe a) where
    Nothing <> y = y
    x       <> y = x
-- this is similar to 'orElse', which is:
--   - in the parser framework in the coursework
--   - in the makeChange function solutions in Week01
--   - it will come up next week's material (not the test)

-- a Monoid is:
--   1. A semigroup
--   2. That has a "zero" element
class Semigroup a => Monoid a where
   mempty :: a

instance Monoid Int where
    mempty = 0

instance Monoid [a] where
    mempty = []

instance Monoid Bool where
    mempty = True 

instance Monoid (Maybe a) where
    mempty = Nothing


-- crush, reduce, fold
crush :: Monoid a => [a] -> a
crush []     = mempty
crush (x:xs) = x <> crush xs
-- = foldr (<>) mempty

-- crush becomes super useful if we combine it with 'map'

-- Why?
--   we have a list of things
--   we translate them to something in a monoid, using 'map'
--   we then crush them all to summarise

find :: Eq k => k -> [(k,v)] -> Maybe v
find key kvs = crush (map (\(k,v) -> if k == key then Just v else Nothing) kvs)

-- This "pattern" is the essence of the Map-Reduce technique popularised by Google in the mid 2000s
--
-- - The 'map' can be done in parallel, potentially on many machines
-- - Monoids have the property that it doesn't matter what order things are combined in:
--     a <> (b <> c) == (a <> b) <> c   (associativity)
--   which means that combining or crushing or reducing can be done in parallel chunks, in a tree layout.

-- This basic idea can be generalised:

-- Foldable -- is a type class that expresses the ability to 'crush' or 'reduce' or 'fold' a collection
--    Lists are Foldable
--    Trees are Foldable
--    in general, any container type is foldable


-- Functor -- is a type class that expresses the ability to 'map'
--    Lists are Functors
--    Trees are Functors
--    in general, any container type are is a Functor

-- Functor is a difficult word: should be "Mappable"



