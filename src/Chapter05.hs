{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use isAsciiLower" #-}

module Chapter05 where

import Prelude (
    Bool (False, True),
    Char,
    Eq,
    Int,
    Ord ((<=)),
    String,
    and,
    even,
    mod,
    sum,
    tail,
    take,
    zip,
    (!!),
    (&&),
    (+),
    (==),
    (>=),
    (^),
 )

-- LIST COMPREHENSIONS

{- | In mathematics, the *comprehension* notation can be used to construct
new sets from existing sets.

`|` is read as "such that"
`<-` is read as "is drawn from"
`x <- [1..5]` is called a generator
>>> [x^2 | x <- [1..5]]
[1,4,9,16,25]

A list comprehension can have more than one generator.

>>> [(x,y) | x <- [1,2,3], y <- [4,5]]
[(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]

Changing the order of these 2 generators produces the same set of pairs,
but arranged in a different order.

>>> [(x,y) | y <- [4,5], x <- [1,2,3]]
[(1,4),(2,4),(3,4),(1,5),(2,5),(3,5)]

Later generators can also depend upon the value of variables from early generators.
>>> [(x,y) | x <- [1..3], y <- [x..3]]
[(1,1),(1,2),(1,3),(2,2),(2,3),(3,3)]
-}

{- | A more practical example of this idea, we can replicate the prelude functionality of `concat`.
By using one generator to select each list in turn, and another to select each element from each list.

>>> concat [[1,2], [3,4], [5]]
[1,2,3,4,5]
-}

-- 🤯
concat :: [[a]] -> [a]
concat xss = [x | xs <- xss, x <- xs]

{- | The wildcard pattern `_` is sometimes useful in generators, to discard certain elements from a list.
For example, a function that selects all the first components from a list of pairs can be defined as follows.

>>> firsts [(1, 'a'), (2, 'b'), (3, 'b')]
[1,2,3]
-}
firsts :: [(a, b)] -> [a]
firsts ps = [x | (x, _) <- ps]

{- | Similarly, the library function that calculates the length of a list can be defined
by replacing each element by one and summing the resulting list.

>>> length ['a', 'b', 'c', 'd']
4
-}
length :: [a] -> Int
length xs = sum [1 | _ <- xs]

-- GUARDS

{- | List comprehensions can also use logical expressions called *guards*, to filter the values produced by
earlier generators. If a guard is True, then the current values are retained, if it is False, then they are discarded.

For example:
>>> [x | x <- [1..10], even x]
[2,4,6,8,10]

Similarly, here's a function that maps a positive integer to a list of positive factors:
>>> factors 15
[1,3,5,15]

>>> factors 7
[1,7]
-}
factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

{- | Recall that an integer greater than one is *prime* if its only positive factors are one and the number itself.
Hence, we can devise a simple `prime` function as follows:

Note that the lazy evaluation stops as soon as [1,3.. is produced.
>>> prime 15
False

>>> prime 7
True
-}
prime :: Int -> Bool
prime n = factors n == [1, n]

{- | We can now define a function that produces the list of all prime numbers up to a given limit:

>>> primes 40
[2,3,5,7,11,13,17,19,23,29,31,37]
-}
primes :: Int -> [Int]
primes n = [x | x <- [2 .. n], prime x]

{- | As a final example concerning guards, suppose that we can represent a lookup table as a list of key value pairs.
We then find the values based on the matching keys.

>>> find 'b' [('a', 1), ('b', 2), ('c', 3), ('d', 4), ('b', 5)]
[2,5]

>>> find 'z' [('a', 1), ('b', 2), ('c', 3), ('d', 4), ('b', 5)]
[]
-}
find :: (Eq a) => a -> [(a, b)] -> [b]
find k lst = [v | (k', v) <- lst, k == k']

-- THE ZIP FUNCTION

{- | The library function `zip` produces a new list by pairing successive elements
from two existing lists until either or both lists are exhausted.

For example

>>> zip ['a', 'b', 'c'] [1, 2, 3, 4]
[('a',1),('b',2),('c',3)]
-}

{- | It is often useful when programming with list comprehension.
For instance, suppose that we define a function that returns the list of all
pairs of adjacent elements from a list as follows

>>> pairs []
[]

>>> pairs [1]
[]
>>> pairs [1,2]
[(1,2)]

>>> pairs [1,2,3]
[(1,2),(2,3)]

>>> pairs [1,2,3,4]
[(1,2),(2,3),(3,4)]
-}
pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

{- | Then using `pairs` we can now define a function that decides if a list of
elements of any ordered type is sorted by simply checking that all pairs of
adjacent elements from the list are in a correct order.

>>> and []
True

>>> and [True, True, True]
True

>>> and [True, True, False]
False

---

>>> sorted [1,2,3,4]
True

>>> sorted [1,3,2,4]
False
-}

-- 🤯
-- Similarly to the function `prime`, computation stops as soon as possible.
sorted :: (Ord a) => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]

{- | Using `zip`, we can also define a function that returns the list of all positions
at which a value occurs in a list, by pairing each element with its position, and selecting
those positions at which the desired value occurs:

>>> positions False [True, False, True, False]
[1,3]

>>> :{
 let xs = [True, False, True, False] in
 zip xs [0 ..]
:}
[(True,0),(False,1),(True,2),(False,3)]
-}
positions :: (Eq a) => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0 ..], x == x']

-- STRING COMPREHENSIONS

{- | String are in fact a [Char].
For example `"abc" :: String` is an abbreviation for `['a', 'b', 'c'] :: [Char]`.
Because strings are lists, any polymorphic function on lists can also be used with strings.

>>> "abcde" !! 2
'c'

>>> take 3 "abcde"
"abc"

>>> length "abcde"
5

>>> zip "abc" [1,2,3,4]
[('a',1),('b',2),('c',3)]
-}

{- | For the same reason, list comprehensions can also be used to define functions on strings:

>>> lowers "Haskell"
6
-}
lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

{- | >>> count 'l' "hello"
2
-}
count :: Char -> String -> Int
count x xs = length [x | x' <- xs, x == x']

-- THE CAESAR CIPHER
-- See Chapter05.Caesar