{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use list literal pattern" #-}
{-# HLINT ignore "Use list literal" #-}
{-# HLINT ignore "Use foldr" #-}
module Chapter06.Exercises where

import Prelude hiding (and, concat, elem, replicate, (!!), (^))

{- |

1) How does the recursive version of the factorial function behave if applied
   to a negative argument, such as (-1)?

   Modify the definition to prohibit negative arguments by adding a guard to the recursive case.


The function enters an infinite recursion

>>> fact 4
24

>>> fact (-4)
*** Exception: negative input
...
-}
fact 0 = 1
fact n
   | n < 0 = error "negative input"
   | otherwise = n * fact (n - 1)

-------------------------------------------------------------------------------

{- |

2) Define a recursive function `sumDown :: Int -> Int` that returns the sum of
   the non-negative integers from a given value down to zero.

   For example `sumDown 3` should return the result 3+2+1+0 = 6.

>>> sumDown 3
6
-}
sumDown :: Int -> Int
sumDown 0 = 0
sumDown n
   | n < 0 = error "negative input"
   | otherwise = n + sumDown (n - 1) -- this version can overflow the stack

-------------------------------------------------------------------------------

{- |

3) Define the exponentiation operator `^` for non-negative integers using the
   same pattern of recursion as the multiplication operator `*`, and show how
   the expression `2 ^ 3` is evaluated using your definition.

>>> 2^0
1

>>> 2^1
2

>>> 2^2
4

>>> 2^3
8

>>> 2^(-1)
*** Exception: negative
...
-}

-- (^) :: (Eq n, Num n) => n -> n -> n
(^) :: (Ord n, Num n) => n -> n -> n
n ^ 0 = 1
n ^ 1 = n
n ^ exp
   | exp < 0 = error "negative"
   | otherwise = n * n ^ (exp - 1)

-------------------------------------------------------------------------------

{- |

4) Define a recursive function `euclid :: Int -> Int -> Int` that implements
   *Euclid's algorithm* for calculating the greatest common divisor of two non
   negative integers: if the two numbers are equal, this number is the result;
   otherwise, the smaller number is subtracted from the larger, and the same
   process is then repeated.

>>> euclid 5 5
5

>>> euclid 6 27
3

>>> euclid 1024 48
16
-}
euclid :: (Ord a, Num a) => a -> a -> a
euclid a b
   | a == b = a
   | a < b = euclid a (b - a)
   | a > b = euclid (a - b) b

-------------------------------------------------------------------------------

{-

5) Show how recursive functions are evaluated

length [1,2,3]
= 1 + length [2,3]
= 1 + 1 + length [3]
= 1 + 1 + 1 + length []
= 1 + 1 + 1 + 0
= 3

drop 3 [1,2,3,4,5]
= drop 2 [2,3,4,5]
= drop 1 [3,4,5]
= drop 0 [4,5]
= [4,5]

init [1,2,3]
= 1 : init [2,3]
= 1 : 2 : init [3]
= 1 : 2 : []
= [1,2]

 -}

-------------------------------------------------------------------------------

{- |

6) Define prelude functions

>>> and []
True

>>> and [True]
True
>>> and [True, False]
False

-- Alternative 1
and :: [Bool] -> Bool
and [] = True
and (x : xs) = x && (and xs)

-- Alternative 2
and = foldl (&&) True
-}
and :: [Bool] -> Bool
and [] = True
and (False : _) = False
and (True : xs) = and xs

{- |

>>> concat [[]]
[]

>>> concat [[1]]
[1]

>>> concat [[1,2]]
[1,2]

>>> concat [[1],[2]]
[1,2]

>>> concat [[1],[2,3]]
[1,2,3]

>>> foldl (++) [] $ [[1], [2,3], [4,5,6]]
[1,2,3,4,5,6]

>>> foldl (++) [] $ [[]]
[]
-}
concat :: [[a]] -> [a]
concat ([] : []) = []
concat (xs : []) = xs
concat ((x : xs) : xss) = x : concat (if null xs then xss else xs : xss)

{- |

>>> concat' [[]]
[]

>>> concat' [[1]]
[1]

>>> concat' [[1,2]]
[1,2]

>>> concat' [[1],[2]]
[1,2]

>>> concat' [[1],[2,3]]
[1,2,3]

concat' t = foldl (++) [] t
-}
concat' :: [[a]] -> [a]
concat' [] = []
concat' (h : t) = h ++ concat' t

{- |

>>> replicate 0 1
[]

>>> replicate 3 1
[1,1,1]

>>> replicate 3 'a'
"aaa"
-}

-- replicate :: (Num n, Ord n) => n -> a -> [a]
replicate :: (Num n, Ord n) => n -> a -> [a]
replicate 0 _ = []
replicate n x
   | n < 0 = error "negative"
   | otherwise = x : replicate (n - 1) x

{- |

>>> [] !! 0
*** Exception: invalid index
...

>>> [1,2,3] !! (-1)
*** Exception: negative
...

>>> [1,2,3] !! 0
1

>>> [1,2,3] !! 1
2

>>> [1,2,3] !! 2
3

>>> [1,2,3] !! 99
*** Exception: invalid index
...
-}
(!!) :: (Num n, Ord n) => [a] -> n -> a
[] !! _ = error "invalid index"
(x : _) !! 0 = x
(_ : xs) !! n
   | n < 0 = error "negative"
   | otherwise = xs !! (n - 1)