{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use sum" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use product" #-}
{-# HLINT ignore "Use or" #-}
{-# HLINT ignore "Use and" #-}
{-# HLINT ignore "Use odd" #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Chapter07 where

import Prelude hiding (and, filter, length, map, odd, or, product, reverse, sum, (.))

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

-------------------------------------------------------------------------------
-- FOLD RIGHT
-------------------------------------------------------------------------------

{- |
Many functions that talk a list as their argument can be defined using the
following simple pattern of recursion on lists:

f [] = v
f (x:xs) = x # f xs

That is, the function maps to the empty list to a value `v`, and any non-empty
list to an operator `#` applied to the head of the list and the result of
recursively processing the tail.

Here are a few functions that follow this pattern

>>> sum [1..10]
55

>>> product [1..6]
720

1 × 2 × 3 × 4 × 5 × 6 = 720

>>> or [True,False]
True

>>> or [False,True]
True

>>> or [True,True]
True

>>> or [False,False]
False

>>> and [False,False]
False

>>> and [True,False]
False

>>> and [False,True]
False

>>> and [True,True]
True
-}

-- We saw these `sum` and `product` definition in chapter 1 and 6
sum :: (Num a) => [a] -> a
sum [] = 0
sum (x : xs) = x + sum xs

product :: (Num a) => [a] -> a
product [] = 1
product (x : xs) = x * product xs

or :: [Bool] -> Bool
or [] = False
or (x : xs) = x || or xs

and :: [Bool] -> Bool
and [] = True
and (x : xs) = x && and xs

{- |
The higher order function `foldr` encapsulates this pattern, with the operator
`#` and the value `v` as arguments

>>> sum' [1..10]
55

>>> product' [1..6]
720

1 × 2 × 3 × 4 × 5 × 6 = 720

>>> or' [True,False]
True

>>> or' [False,True]
True

>>> or' [True,True]
True

>>> or' [False,False]
False

>>> and' [False,False]
False

>>> and' [True,False]
False

>>> and' [False,True]
False

>>> and' [True,True]
True
-}
sum' :: (Foldable t, Num a) => t a -> a
sum' xs = foldr (+) 0 xs

product' :: (Foldable t, Num a) => t a -> a
product' xs = foldr (*) 1 xs

or' :: (Foldable t) => t Bool -> Bool
or' xs = foldr (||) False xs

and' :: (Foldable t) => t Bool -> Bool
and' xs = foldr (&&) True xs

{- | We could also define `foldr` in terms of recursion

>>> foldr' (+) 0 [1,2,3,4]
10

This definition isn't as general as the standard prelude's definition though.
We can't pattern match like this over a generic Foldable structure.
Thus, this definition only applies to list, not generic foldable structures (trees, arrays, etc.)
-}
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v [] = v
foldr' f v (x : xs) = f x (foldr' f v xs)

{- |

It may be useful to think about this function, not in terms of recursion but in terms of a list,
where each cons operation is replaced by `f`, and the empty list at the end replaced by `v`.

>>> 1 : (2 : (3 : []))
[1,2,3]

>>> 1 + (2 + (3 + 0))
6
-}

{- | We can implement many functions with this pattern


>>> length [1,2,3]
3
-}
length :: (Num a) => [a] -> a
length [] = 0
length (_ : xs) = 1 + length xs

{-

It is equivalent to this transformation:
> 1 : (2 : (3 : []))
> 1 + (1 + (1 +  0))

In other words, we replace each (:) operation with `(+)` and `[]` with `0`.

Thus the definition of `length'`
 -}

length' :: (Foldable t, Num b) => t a -> b
length' = foldr (\_ n -> 1 + n) 0

{- |

Now let's implement `reverse` with recursion

>>> reverse "abc"
"cba"
-}
reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

{- |

Applying reverse to this list

> 1 (2 : (3 : []))
> [] ++ [3] ++ [2] ++ [1]

If it's not that clear, let's implement `snoc`

>>> reverse' "abc"
"cba"

>>> reverse'' "abc"
"cba"
-}
snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = snoc x $ reverse xs

reverse'' :: [a] -> [a]
reverse'' = foldr snoc []

-------------------------------------------------------------------------------
-- FOLD LEFT
-------------------------------------------------------------------------------

{- |

>>> sum'' [1..10]
55
-}
sum'' :: [Integer] -> Integer
sum'' = sum_ 0
  where
    sum_ acc [] = acc
    sum_ acc (x : xs) = sum_ (acc + x) xs

{-

Fold right
==========
sum (x : xs) = add x (sum xs)
sum (x : xs) = x + (sum xs)

Fold left -> we need an accumulator!
=========
sum acc (x : xs) = sum (acc + x) xs

---

Fold right
==========
f []     = v
f (x:xs) = x # f xs

Fold left
=========
f acc []     = acc
f acc (x:xs) = f (acc # x) xs

 -}

{-

In other words:

Folding left looks something like this
--------------------------------------
sum 0 [1,2,3]
sum 1 [2,3]
sum 3 [3]
sum 6 []
6

Folding right looks something like this
------------------------------------
sum [1,2,3]
> add 1 (sum [2,3])
> add 1 (add 2 (sum [3])))
> add 1 (add 2 (add 3 (sum []))))
> add 1 (add 2 (add 3 0)))
> add 1 (add 2 3))
> add 1 5
6

======

We can think of foldl/foldr operations this way:

Fold left
---------
foldl (#) acc [x0, x1, x2, x3] = (((acc # x0) # x1) # x2) # x3
foldl (#) acc [x0, x1, x2, x3] = acc # x0 # x1 # x2 # x3

Parenthesis are optional
foldl (-) 0 [1,2,3] = (((0 - 1) - 2) - 3)
                    = -6

Fold right
----------
foldr (#) acc [x0, x1, x2, x3] = x0 # (x1 # (x2 # (x3 # acc)))

We have to put parentheses
foldr (-) 0 [1,2,3] = 1 - (2 - (3 - 0))
                    = 1 - (2 - 3)
                    = 1 - (-1)
                    = 1 + 1
                    = 2
 -}

-------------------------------------------------------------------------------
-- FUNCTION COMPOSITION
-------------------------------------------------------------------------------

-- With the function composition operator, we can write simpler function definitions

(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)

odd :: (Integral a) => a -> Bool
odd n = not (even n)

odd' :: (Integral a) => a -> Bool
odd' = not . even

twice' :: (a -> a) -> a -> a
twice' f x = f (f x)

twice'' :: (a -> a) -> a -> a
twice'' f = f . f

{-
Defined earlier
sumSquareEven :: [Int] -> Int
sumSquareEven ns = sum (map (^ 2) (filter even ns))
 -}

sumSquareEven' :: [Int] -> Int
sumSquareEven' = sum . map (^ 2) . filter even

-- We can also compose a list of functions like such:
compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

{- |

>>> twice ((:) 0) [3]
[0,0,3]

>>> compose [((:)1),((:)2)] $ [3]
[1,2,3]

>>> compose [map (+10), map (*2)] [1,2,3]
[12,14,16]


>>> map (+10) . map (*2) $ [1,2,3]
[12,14,16]
-}
