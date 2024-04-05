{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Chapter06 where

import Debug.Trace (trace, traceShow, traceShowId)

{-

Example usage:

drop' n (_ : xs) = drop' (dbg "n" n - 1) (dbg "xs" xs)
 -}
dbg :: (Show a) => [Char] -> a -> a
dbg str x = trace (str ++ "=" ++ show x) x

{- | As we have seen, functions can be defined in terms of other functions

>>> fac 3
6

>>> fac 4
24
-}
fac :: Int -> Int
fac n = product [1 .. n]

{- | In Haskell, it is also permissible to define functions in terms of themselves

>>> fac' 3
6

>>> fac' 4
24
-}
fac' :: Int -> Int
fac' 0 = 1 -- base case
fac' n = n * fac (n - 1) -- recursive case
{-
`fac' 4` translates to:

fac 4 = 4 * fac 3
fac 4 = 4 * (3 * fac 2)
fac 4 = 4 * (3 * (2 * fac 1))
fac 4 = 4 * (3 * (2 * (1 * fac 0)))
fac 4 = 4 * (3 * (2 * (1 * 1)))
fac 4 = 4 * (3 * (2 * 1))
fac 4 = 4 * (3 * 2)
fac 4 = 4 * 6
fac 4 = 24
 -}

{- | We could define recursion on lists

>>> product' [2,3,4]
24
-}
product' :: (Num a) => [a] -> a
product' [] = 1
product' (n : ns) = n * product' ns

{-
`product' [2,3,4]` translates to:

product [2,3,4] = 2 * product [3,4]
product [2,3,4] = 2 * 3 * product [4]
product [2,3,4] = 2 * 3 * 4 * product []
product [2,3,4] = 2 * 3 * 4 * 1
product [2,3,4] = 2 * 3 * 4
product [2,3,4] = 2 * 12
product [2,3,4] = 24
 -}

{- | A few more

>>> length' [1,2,3]
3

>>> reverse' [1,2,3]
[3,2,1]

>>> reverse' "abc"
"cba"
-}
length' :: [a] -> Int
length' [] = 0
length' (_ : xs) = 1 + length' xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

{- | Insertion sort

First we define insert:

>>> insert 0 [1,2,3]
[0,1,2,3]

>>> insert 2 [1,3,4]
[1,2,3,4]

>>> insert 5 [1,2,3]
[1,2,3,5]

Then, we can define iSort:

>>> iSort [3,2,1,4]
[1,2,3,4]


iSort [3,2,1,4] = insert 3 (insert 2 (insert 1 (insert 4 [])))
                = insert 3 (insert 2 (insert 1 [4]))
                = insert 3 (insert 2 [1,4])
                = insert 3 [1,2,4]
                = [1,2,3,4]
-}
insert :: (Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x (y : ys)
  | x < y = x : y : ys
  | otherwise = y : insert x ys

iSort :: (Ord a) => [a] -> [a]
iSort [] = []
iSort (x : xs) = insert x (iSort xs)

{- | We can also apply the recursion on multiple arguments


>>> zip' ['a','b','c'] [1,2,3,4]
[('a',1),('b',2),('c',3)]

zip ['a','b','c'] [1,2,3,4]
  = ('a',1) : zip ['b','c'] [2,3,4]
  = ('a',1) : ('b',2) : zip ['c'] [3,4]
  = ('a',1) : ('b',2) : ('c',3) : zip [] [4]
  = ('a',1) : ('b',2) : ('c',3) : []
  = [('a',1),('b',2),('c',3)]
-}
zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

{- |

>>> drop' 3 [1..9]
[4,5,6,7,8,9]

>>> drop' 99 [1..9]
[]

This could be considered "wrong".
The empty list is returned because we do consume `xs` until it's empty.
>>> drop' (-1) [1..9]
[]
-}
drop' :: (Eq n, Num n) => n -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (_ : xs) = drop' (n - 1) xs

{- | We can apply multiple recursion, where the function is applied more than once in its definition

>>> fib 10
55
-}
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

{- | We can also apply mutual recursion, in which several functions are defined recursively in terms of each other

>>> odd' 32
False

>>> odd' 33
True

>>> even' 4
True

>>> even' 5
False

(traceShow ("[even]" ++ " " ++ show n)
odd' n = even' (trace ("[odd]" ++ " " ++ n) (n - 1))
-}
even' :: (Eq a, Num a) => a -> Bool
even' 0 = True
even' n = odd' (n - 1)

odd' :: (Eq a, Num a) => a -> Bool
odd' 0 = False
odd' n = even' (n - 1)

{-

Debug version, try it in ghci.

ghci> even'' 2
even: n=2
odd : n=1
even: n=0
True

ghci> odd'' 2
odd : n=2
even: n=1
odd : n=0
False

 -}
even'' :: (Eq a, Num a, Show a) => a -> Bool
even'' 0 = trace "even: n=0" True
even'' n = trace ("even: n=" ++ show n) odd'' (n - 1)

odd'' :: (Eq a, Num a, Show a) => a -> Bool
odd'' 0 = trace "odd : n=0" False
odd'' n = trace ("odd : n=" ++ show n) even'' (n - 1)