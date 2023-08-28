{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use splitAt" #-}
{-# HLINT ignore "Use guards" #-}
module Chapter04 where

import Prelude (Bool (False, True), Eq ((==)), Fractional ((/)), Int, Integral (mod), Num, Ord ((<), (>), (>=)), drop, otherwise, take)

{- We can define new functions by combining existing functions
 -}

even :: Integral a => a -> Bool
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
-}
recip :: Fractional a => a -> a
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