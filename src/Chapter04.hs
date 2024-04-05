{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# HLINT ignore "Use splitAt" #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Collapse lambdas" #-}
{-# HLINT ignore "Redundant if" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Chapter04 where

import Prelude (
    Bool (False, True),
    Char,
    Eq ((==)),
    Fractional ((/)),
    Int,
    Integral (mod),
    Num,
    Ord ((<), (>), (>=)),
    div,
    drop,
    foldl,
    head,
    length,
    map,
    null,
    otherwise,
    tail,
    take,
    (!!),
    ($),
    (*),
    (+),
    (-),
    (.),
    (/=),
 )

{- We can define new functions by combining existing functions
 -}

even :: (Integral a) => a -> Bool
even n = n `mod` 2 == 0

{- | Split a list at the nth element (normally a Prelude function)
>>> splitAt 2 [1,2,3,4]
([1,2],[3,4])
-}
splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

{- | Reciprocation (normally a Prelude function)
>>> recip 4
0.25
>>> recip 3 + recip 3
0.6666666666666666
>>> recip 3 + recip 3 + recip 3
1.0
-}
recip :: (Fractional a) => a -> a
recip n = 1 / n

{- =======================
   CONDITIONAL EXPRESSIONS
   ======================= -}

{- | Absolute (normally a Prelude function)
>>> abs 1
1
>>> abs (-1)
1
-}
abs :: (Ord a, Num a) => a -> a
abs n = if n >= 0 then n else -n

{- | Signum (normally a Prelude function)
>>> signum 123
1
>>> signum (-123)
-1

>>> signum 0
0
-}

{- FOURMOLU_DISABLE -}
signum :: Int -> Int
signum n =
    if n < 0 then
        -1
    else
        if n == 0 then
            0
        else
            1
{- FOURMOLU_ENABLE -}

{-

GUARDED EXPRESSIONS
===================

The may improve readability

 -}

{- |

The guarded expression is read like this:

abs of n "such that" n >= 0 ...

The reason for its usage is to improve readability (compared to nested if statements, see signum)

>>> abs' 1
1
>>> abs' (-1)
1
-}
abs' n
    | n >= 0 = n
    | otherwise = -n

{- |
>>> signum' 123
1
>>> signum' (-123)
-1

>>> signum' 0
0
-}

{- FOURMOLU_DISABLE -}
signum' :: Int -> Int
signum' n
    | n > 0     =  1
    | n < 0     = -1
    | otherwise =  0
{- FOURMOLU_ENABLE -}

{-

PATTERN MATCHING
================

Many functions have simple and intuitive definitions using pattern matching.
 -}

not :: Bool -> Bool
not True = False
not False = True

{-

May also be written like this:

(&&) :: Bool -> Bool -> Bool
True && True = True
True && False = False
False && True = False
False && False = False

Or

(&&) :: Bool -> Bool -> Bool
(&&) True True = True
(&&) _ _ = False

Or

(&&) :: Bool -> Bool -> Bool
True && True = True
_ && _ = False

 -}

{- The following version performs better with lazy evaluation because only the
   first argument has to be evaluated to compute something meaningful. -}
(&&) :: Bool -> Bool -> Bool
True && b = b
False && _ = False

{-
TUPLE PATTERNS
==============
 -}

{- |

>>> fst (1, 2)
1
>>> snd (1, 2)
2
-}

-- First (normally a Prelude function)
fst :: (a, b) -> a
fst (x, _) = x

-- Second (normally a Prelude function)
snd :: (a, b) -> b
snd (_, y) = y

{-
LIST PATTERNS
=============
 -}

{- | We can pattern match on specific lengths

>>> test1 ['b', 'c', 'd']
False
>>> test1 ['a', 'c', 'd']
True
-}
test1 :: [Char] -> Bool
test1 ['a', _, _] = True
test1 _ = False

{- | We can pattern match on any length with the cons operator

>>> test2 ['b', 'c', 'd']
False
>>> test2 ['a', 'c', 'd']
True
>>> test2 []
False
-}
test2 :: [Char] -> Bool
test2 ('a' : _) = True
test2 _ = False

{-
LAMBDA EXPRESSIONS
==================
 -}

{- | const is in prelude. It can be defined in terms of a lambda.
>>> map (const 1) [1,2,3]
[1,1,1]
-}
const :: a -> b -> a
const x = \_ -> x

{- | Or, it can be defined in terms of a function ignoring its second argument
>>> map (const' 1) [1,2,3]
[1,1,1]
-}
const' :: a -> b -> a
const' x _ = x

{- | Lambda expressions can be used to avoid having to name a function
     that is only referenced once in a program

>>> odds 5
[1,3,5,7,9]
>>> odds' 5
[1,3,5,7,9]
-}
odds :: Int -> [Int]
odds n = map f [0 .. n - 1]
  where
    f x = x * 2 + 1

odds' :: Int -> [Int]
odds' n = map (\n -> n * 2 + 1) [0 .. n - 1]

{-
OPERATOR SECTIONS
=================
 -}

{- | Any function can be applied infix, like this:

>>> div 12 4
3
>>> 12 `div` 4
3

It's also possible to apply in infix operator postfix, like this:
>>> 12 / 4
3.0
>>> (/) 12 4
3.0

This convention allows the arguments to be included in the parentheses:

>>> (12/) 4
3.0
>>> (/4) 12
3.0

Here's a recap: (a section is what's on the left of the equal sign below)

  (#) = \x -> \y -> x # y
(x #) = \y -> \x -> x # y
(# y) = \x -> \y -> x # y

A quick note:

 (+) is the addition function      : (\x -> \y -> x+y)
(1+) is the successor function     : (\y -> 1+y)
(1/) is the reciprocation function : (\y -> 1/y)
(*2) is the doubling function      : (\x -> x*2)
(/2) is the halving function       : (\x -> x/2)
-}

{- | We can define sum concisely, like such:

>>> sum [1,2,3,4]
10
-}
sum :: [Int] -> Int
sum = foldl (+) 0

-- ================== EXERCISES ==================

{- | 1. Define halve

>>> halve [1,2,3,4,5,6]
([1,2,3],[4,5,6])
-}
halve :: [a] -> ([a], [a])
halve lst =
    let
        half = length lst `div` 2
     in
        (take half lst, drop half lst)

{- | 2. Define third, using 3 methods

>>> third_1a [1,2,3,4,5,6,7,8,9]
3
>>> third_1b [1,2,3,4,5,6,7,8,9]
3
>>> third_2 [1,2,3,4,5,6,7,8,9]
3
>>> third_3a [1,2,3,4,5,6,7,8,9]
3
-}

-- With `head` and `tail`
third_1a :: [a] -> a
third_1a lst = head $ tail $ tail lst

third_1b :: [a] -> a
third_1b = head . tail . tail

-- With the list indexing operator `!!`
third_2 :: [a] -> a
third_2 = (!! 2)

-- With pattern matching
third_3a :: [a] -> a
third_3a (_ : _ : x : _) = x

{- | 3. Implement `safe_tail` in 3 ways

>>> safe_tail_1 []
[]
>>> safe_tail_1 [1,2,3]
[2,3]

>>> safe_tail_2 []
[]
>>> safe_tail_2 [1,2,3]
[2,3]

>>> safe_tail_3 []
[]
>>> safe_tail_3 [1,2,3]
[2,3]
-}

-- With a condition expression
safe_tail_1 lst =
    if null lst
        then []
        else tail lst

-- With a guarded equation
safe_tail_2 lst
    | null lst = []
    | otherwise = tail lst

-- With pattern matching
safe_tail_3 [] = []
safe_tail_3 (_ : xs) = xs

-- | 4. As with `&&`, show how the disjunction operator `||` can be defined in 4 different ways

-- 0/4
(||) :: Bool -> Bool -> Bool
(||) False x = x
(||) True _ = True

-- 1/4
-- (||) False False = False
-- (||) _ _ = True

-- 2/4
-- False || False = False
-- _ || _ = True

-- 3/4
-- False || False = False
-- False || True = True
-- True || False = True
-- True || True = True

-- 4/4
-- (||) False False = False
-- (||) False True = True
-- (||) True False = True
-- (||) True True = True

{- | 5. Formalise the following expression using condition expressions

True && True = True
_    && _    = False

Same as:
-}
and_ a b =
    if a
        then
            if b
                then True
                else False
        else False

{- |  6. Formalise (we should need less branches)
True && b = b
False && _ = False
-}
and_2 a b =
    if a
        then b
        else False

-- | 7. Same as:
mul0 :: Int -> Int -> Int -> Int
mul0 x y z = x * y * z

mul :: Int -> Int -> Int -> Int
mul = \x -> \y -> \z -> x * y * z

{- | 8. Define luhnDouble + luhn

>>> luhnDouble 3
6

>>> luhnDouble 6
3

>>> luhn 1 7 8 4
True

>>> luhn 4 7 8 3
False
-}
luhnDouble :: (Ord a, Num a) => a -> a
luhnDouble n =
    let n' = n * 2
     in if n' > 9
            then n' - 9
            else n'

luhn a b c d = (luhnDouble a + b + luhnDouble c + d) `mod` 10 == 0
