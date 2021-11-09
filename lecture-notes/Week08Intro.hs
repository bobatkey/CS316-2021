module Week08Intro where

import Control.Monad (mapM)
import Week08
import Data.Char (isAlpha)

{- Week 08 : Real I/O and Parser Combinators -}


{- Part the first : Real I/O -}

{- So far,
     *Simulating* side effects: exceptions, state, printing, and processes (Week 07 tutorial qns)

   Haskell also includes facilities for doing "Real" I/O.

   We could add:
      putChar :: Char -> ()   -- void System.out.print(char c)
                              -- void putc(char c)

      getChar :: () -> Char   -- char getc();

   readTwoChars :: String
   readTwoChars = [getChar (), getChar ()]

   makeTwoCharString :: String
   makeTwoCharString = [f 1, f 1]

   Could optimise this to:

   makeTwoCharString :: String
   makeTwoCharString = [x,x]
     where x = f 1

   But if optimise 'readTwoChars', we get:

   readTwoChars :: String
   readTwoChars = [x,x]
     where x = getChar ()

   which does not have the same behaviour!


   Over the last few weeks, we have seen that we can use the *types*
   to isolate parts of the program that have side effects.

   - Maybe / Result for exceptions
   - State for mutable variables
   - Printing for printing

   All of these have a common interface: 'Monad'

   Haskell has a built-in monad called 'IO', which represents actions
   that may perform real input/output.

   That is, a value of type 'IO a' is a description of an action to be
   performed that will result in a value of type 'a'.

   The actual performance of that action is done by the Haskell system
   (or "runtime").

   So all Haskell I/O commands have types that involve 'IO':

     getChar :: IO Char         -- an IO action that returns a 'Char'
     putChar :: Char -> IO ()   -- takes a 'Char', and returns an IO action that returns the empty tuple
-}

readTwoChars :: IO String
readTwoChars = do c1 <- getChar
                  c2 <- getChar
                  return [c1, c2]

printString :: String -> IO ()
printString = mapM_ (\c -> putChar c)



{- Part the second : Parser Combinators -}

{- Now that we can read in some text, we (the Haskell program) needs to
   be able to understand it. -}

-- For demonstration purposes, we will write a parser for
-- 'S-expressions' (a.k.a. Lisp code).

-- <expr> ::= <atom>
--          | '(' <atom> (sp* <expr>)* ')'

-- For example:
--    (if (eq a b) (launchmissiles) (petdog))

data SExp
  = Atom String
  | Appl String [SExp]
  deriving Show

atomChar :: Parser Char
atomChar = do c <- char
              if isAlpha c then return c else failParse

parseAtom :: Parser String
parseAtom = do c  <- atomChar
               cs <- zeroOrMore atomChar
               return (c:cs)

parseArg :: Parser SExp
parseArg = do zeroOrMore (isChar ' ')
              parseSExp

parseSExp :: Parser SExp
parseSExp = do atom <- parseAtom
               return (Atom atom)
            `orElse`
            do isChar '('
               atom <- parseAtom
               args <- zeroOrMore parseArg
               isChar ')'
               return (Appl atom args)
