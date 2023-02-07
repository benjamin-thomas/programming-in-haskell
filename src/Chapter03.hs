{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Chapter03 where

{- | Int represents fixed-precision integers, from 2^-63 to 2^63-1

This is okay:
>>> 2 ^ 63 -1 :: Int
9223372036854775807

But this gives the wrong answer due to overflow!
>>> 2 ^ 63 :: Int
-9223372036854775808

Behavior is odd afterwards
>>> 2 ^ 64 :: Int
0
>>> 2 ^ 65 :: Int
0
-}

{- | Integers represents arbitrary-precision integers, no limits but less performant


As with Int, this is okay:
>>> 2 ^ 63 -1 :: Integer
9223372036854775807

No overflow here
>>> 2 ^ 63 :: Integer
9223372036854775808
>>> 2 ^ 64 :: Integer
18446744073709551616
>>> 2 ^ 65 :: Integer
36893488147419103232
-}


-- Polymorphic functions

{- | `zip` (and many others) is said to be "polymorphic"
>>> zip [1,2,3] ["one", "two", "three"]
[(1,"one"),(2,"two"),(3,"three")]
-}

-- Overloaded types, overloaded functions

{- | Considering: `(+) :: Num a => a -> a ->`

It is said that this functions types includes a **class constraint**.

Here, `Num` is the class, and type `a` represents an **instance of the class Num**.

If a type contains a class constraint, it is said to be "overloaded". By extension,
the function is also said to be "overloaded".

>>> (+) 1 2
3

>>> (+) 1.0 2.0
3.0

Numbers themselves are overloaded, i.e. they could be integers or floats depending on
the context in which it is used.

>>> (+) 1 2.0
3.0
-}

-- CLASSES

{-

A **type** (e.g. Int) is a collection of values (e.g. 1,2,3...)
A **class** (e.g. Num) is a collection of types (Int, Integer, Float, Double...)
A **method** (e.g. (==)) is an overloaded method, defined on one or many classes (e.g. Int, Char, Bool...)

Bool, Char, String, Int, Integer, Float and Double are **instances** of the `Eq` **class**.

>>> False == False
True

>>> 'A' == 'B'
False

In general, function types are not instances of the `Eq` class. It is not generally feasible to compare 2
functions for equality.
>>> ("one", 1) == ("ONE", 2)
False


Bool, Char, String, Int, Integer, Float and Double are also instances of the `Ord` class (can be ordered).

>>> 1 < 2
True

>>> True < False
False

>>> min True False
False

>>> max 'A' 'B'
'B'

>>> "abc" < "def"
True

>>> "ba" < "abc"
False

>>> [1,2] < [1,2,3]
True

>>> ('a', 1) < ('a', 1)
False

>>> ('a', 1) < ('b', 1)
True

-}


{- | Show class

>>> show 1
"1"

>>> show False
"False"

>>> show [1,2,3]
"[1,2,3]"

>>> show ('a', 1)
"('a',1)"

-}

{- | The read class is the invert of show

>>> read "1" :: Int
1

>>> read "('a', 1)" :: (Char, Int)
('a',1)

-}

{- |
Num implements (+) (-) (*) `negate` `abs` and `signum`.
Integral implements `div` and `mod` over Num
Fractional implements (/) and `recip` over Num

>>> 3 / 2
1.5

>>> 3 `div` 2
1

>>> 3 `mod` 2
1
-}

-- EXERCISES

{-

1. What are the types of the following values?

['a','b','c'] => [Char]
('a','b','c') => (Char, Char, Char)
[(False,'0'),(True),'1'] => [(Bool, Char)]
([False,True],['0','1']) => ([Bool], [Char])
[tail, init, reverse] => [[a] -> [a]]
-}

{-

2. Write down definitions that have the following types; it does not matter what the
   definitions actually do as long as they are type correct

   ...
-}

bools :: [Bool]
bools = [True, False]

nums :: [[Int]]
nums = [[1],[2],[3]]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a, a)
copy x = (x, x)

apply :: (a -> b) -> a -> b
apply f x = f x


{-

3. What are the types of the following functions?

Functions written first as comments, with their type sigs, then checked via the LSP editor integration.
-}

second :: [a] -> a
second xs = head (tail xs)

swap :: (a, b) -> (b, a)
swap (x,y) = (y,x)

pair :: a -> b -> (a, b)
pair x y = (x,y)

double :: Num a => a -> a
double x = x*2

-- palindrome :: [Char] -> Bool
palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)

{- 4. Check your answers: done with the LSP integration. -}

{-

5. Why is it not feasible in general for function types to be instances of the `Eq` class?

   A function may accept other function as input parameters. It is not possible to test the equality of 2 functions.

   When is it feasible?

   When the output of a function is a value.

-}