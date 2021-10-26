module Week06Intro where
import GHC.Maybe (Maybe(Nothing))
import GHC.Generics (V1)


{- Feedback comments:

   - At least two people would like a video introduction to how to get started with
     the coursework
    - I'll try to get this done before the end of next week.

   - Possible further Haskell resources
     - Learn you a Haskell for Great Good
     - "Programming in Haskell" by Graham Hutton
     - Videos by Graham Hutton, which I link from the MyPlace page
-}


{-  WEEK 06 : SIMULATING SIDE EFFECTS

    In Haskell

      f :: Int -> Int
    
    One of two things may happen:
      1. It returns an Int
      2. Or it does not terminate
     (3. It can crash with a unrecoverable error)

    In (a language like) Java

      public static int f(int x)

    If we call this, then:
      1. It returns an Int
      2. Or it does not terminate
      3. Or it throws an exception
      4. And it may print stuff to the screen
      5. Send requests over the network
      6. Make any connected hardware do anything
      7. Launch nuclear missiles
      8. Depend on the current state of the world

   However, we /do/ want to do these things!

   So in Haskell, we:
     (a) simulate (or program) the ability to do these
     (b) built in facility for doing "real world" effects

   In Haskell, we record (or label) the kinds of the side-effects
   that a function may do in its type. -}


{- Simulating Exceptions via Maybe -}

{- data Maybe a
     = Nothing
     | Just a
     deriving (Eq, Show)
-}

returnOk :: a -> Maybe a
returnOk x = Just x

failure :: Maybe a
failure = Nothing

search :: Eq k => k -> [(k,v)] -> Maybe v
search k [] = failure -- if we don't find it, fail with an exception
search k ((k',v'):kvs) =
      if k == k' then
          returnOk v' -- if we do find it, succeed and return normally
      else
          search k kvs

lookupList :: Eq k => [(k,v)] -> [k] -> Maybe [v]
lookupList store [] = returnOk []
lookupList store (k:ks) =
    case search k store of
        Nothing -> -- key not found case
          failure
        Just v ->  -- key found case
            case lookupList store ks of
              Nothing -> failure
              Just vs ->
                  returnOk (v:vs)

ifOk :: Maybe a -> (a -> Maybe b) -> Maybe b
ifOk c k = case c of
  Nothing -> failure
  Just a -> k a

lookupList_v2 :: Eq k => [(k,v)] -> [k] -> Maybe [v]
lookupList_v2 store [] = returnOk []
lookupList_v2 store (k:ks) =
    search k store         `ifOk` \v ->
    lookupList_v2 store ks `ifOk` \vs ->
    returnOk (v:vs)

{- Summary:

    1. We mark (or record, or label) in the type whether or not a function may
       fail with an exception. If it ends with 'Maybe XXX' then it is a function
       that may fail.

    2. We can make the code look more "normal" by defining a helper function 'ifOk'.
 
       - this is (very) common pattern

    In the videos:
      - how to implement 'catch'

    In the tutorial problems:
      - how to add error messages, using the 'Result' type
-}


{- Simulating State via state-passing -}

{- In Java:

     int i = 0;

     i = i + 1;

     ...

     i = i + 10;
-}

numberList :: [a]             -- "normal" input to the function
           -> Int             -- the initial state
           -> ( Int           -- return the final state,
              , [(a,Int)]     --     and the numbered list
              )
numberList []     i = (i, []) 
numberList (x:xs) i =
    let y       = (x,i)
        (i',ys) = numberList xs (i+1)
    in
        (i', y:ys)

{- To make this prettier:

   1. We define a type abbreviation (or synonym) for state passing functions
   2. We define some helper functions to help us write the code
-}

type State a = Int          -- takes an initial state
            -> ( Int        -- returns final state, and
               , a)         --    a value

returnSt :: a -> State a
        --  a -> Int -> (Int, a)
returnSt a s = (s, a)

andThen :: State a          -- first stateful computation to do
        -> (a -> State b)   -- a second stateful computation to do, depending on the result of the first one
        -> State b          -- a complete stateful computation combining both of them
andThen comp kontinuation i =
    let (i', a) = comp i
        (i'', b) = kontinuation a i'
    in
        (i'', b)

get :: State Int
get i = (i, i)

put :: Int -> State ()
put i' i = (i', ())

numberList_v2 :: [a] -> State [(a,Int)]
numberList_v2 [] = returnSt []
numberList_v2 (x:xs) =
    -- 1. get the current state
    get              `andThen`  \i ->
    -- 2. update the current state to add 1
    put (i+1)        `andThen` \() ->
    -- 3. call 'numberList_v2' on xs
    numberList_v2 xs `andThen` \ys ->
    -- 4. put the output list together
    returnSt ((x,i) : ys)

{- Summary:

   1. We use the types to tell us what kind of side effect a function may perform:
        
        - Maybe a    --- exceptions
        - State a    --- imperative, or mutable state
        - Printing a --- doing printing
        - Process a  --- doing I/O

   2. Sequencing function:

        State a -> (a -> State b) -> State b

      (same for Maybe, Printing, Process)
-}







