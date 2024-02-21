{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use map" #-}
module Chapter07 where

import Prelude hiding (filter, map)

-- ghcid -c 'cabal repl' -T ':!doctest ./src/Chapter07.hs'
-- echo ./src/Chapter07.hs | entr -c doctest /_

-- Thanks to automatic currying, this function:
add :: Int -> Int -> Int
add x y = x + y

-- Actually means this:
add' :: Int -> (Int -> Int)
add' = \x -> \y -> x + y

-- In Haskell, it is also permissible to define functions that take functions as arguments
-- Since `twice` takes a function as a parameter and returns another function it is a so-called
-- "higher-order function"
twice :: (a -> a) -> a -> a
twice f x = f (f x)

{- | A higher-order function is a function that takes a function as a parameter
     and returns another function. For example:

>>> twice (*2) 3
12

>>> twice reverse [1,2,3]
[1,2,3]
-}

-------------------------------------------------------------------------------
-- PROCESSING LISTS : map
-------------------------------------------------------------------------------

{- | The standard prelude defines a number of (generic) higher-order functions

>>> map (+1) [1,3,5,7]
[2,4,6,8]

>>> map even [1,2,3,4]
[False,True,False,True]

>>> map reverse ["abc","def","ghi"]
["cba","fed","ihg"]

>>> map' (+1) [1,3,5,7]
[2,4,6,8]
-}
map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x : xs) = f x : map f xs

-- Interestingly, the list-comprehension version is faster (16s vs 20s when inputting a big list)
map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

{- | Further points about map:

1) It's a polymorphic function that can be applied to lists of any type (as seen above).
2) It can be applied to itself to process nested lists

>>> map (map (+1)) [[1,2,3],[4,5]]
[[2,3,4],[5,6]]

Evaluation steps:
> map (map (+1)) [[1,2,3],[4,5]]
> [map (+1) [1,2,3], map (+1) [4,5]]
> [[2,3,4],[5,6]]
-}

-------------------------------------------------------------------------------

filter :: (a -> Bool) -> [a] -> [a]
filter f [] = []
filter f (x : xs) =
    if f x
        then x : filter f xs
        else filter f xs

{- |
>>> filter even [1..10]
[2,4,6,8,10]

>>> filter (> 5) [1..10]
[6,7,8,9,10]

>>> filter (/= ' ') "won der ful"
"wonderful"
-}
filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = [x | x <- xs, f x]

{- |
>>> filter' even [1..10]
[2,4,6,8,10]

>>> filter' (> 5) [1..10]
[6,7,8,9,10]

>>> filter' (/= ' ') "won der ful"
"wonderful"
-}

-- The book's version
filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p [] = []
filter'' p (x : xs)
    | p x = x : filter p xs
    | otherwise = filter p xs

{- |
>>> filter'' even [1..10]
[2,4,6,8,10]

>>> filter'' (> 5) [1..10]
[6,7,8,9,10]

>>> filter'' (/= ' ') "won der ful"
"wonderful"
-}

-------------------------------------------------------------------------------

{- | map+filter are often used in tandem

Example: a function that returns the sum of the squares of the even integers from a list

>>> sumSquareEven [1..10]
220

>>> sum $ map (^2) $ filter even [1..10]
220
-}
sumSquareEven :: [Int] -> Int
sumSquareEven ns = sum (map (^ 2) (filter even ns))

{- | Let's look at a few other higher-order functions that are defined in the standard prelude

>>> all even [2,4,6,8]
True

>>> any odd [2,4,6,8]
False

>>> takeWhile even [2,4,6,1,8,10]
[2,4,6]

>>> dropWhile (< 4) [1, 2, 3, 4, 5, 6, 1, 2, 3]
[4,5,6,1,2,3]
-}
