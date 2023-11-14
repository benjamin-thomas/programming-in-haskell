module Chapter05.Exercises where

import Prelude hiding (replicate)

{-
ghcid -c 'cabal repl' -T ':!doctest ./src/Chapter05/Exercises.hs'
ghcid -c 'cabal repl' -T ':!doctest ./src/'
 -}

{- | 1. Using a list comprehension, give an expression that calculates the sum
1^2 + 2^2 + ...100^2 of the first one hundred integer squares

>>> sumOfSquares 3
[1,4,9]

>>> sumOfSquares 10
[1,4,9,16,25,36,49,64,81,100]
-}
sumOfSquares :: (Num a, Enum a) => a -> [a]
sumOfSquares n = [x ^ 2 | x <- [1 .. n]]

{- | 2. Define the function grid (using list comprehensions)

>>> grid 1 2
[(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]
-}
grid :: Int -> Int -> [(Int, Int)]
grid n m = [(x, y) | x <- [0 .. n], y <- [0 .. m]]

{- | 3. Using grid and list comprehensions, define `square` that returns a
coordinate square of size `n`, excluding the diagonal from (0,0) to (n, n).

>>> square 2
[(0,1),(0,2),(1,0),(1,2),(2,0),(2,1)]
-}
square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

{- | 4. Define replicate

>>> replicate 3 True
[True,True,True]
-}
replicate :: Int -> a -> [a]
replicate n x = [x | _ <- [0 .. (n - 1)]]

{- | 5. Return triples for which the Pythagorean theorem holds (x² + y² = z+²)

>>> pyths 10
[(3,4,5),(4,3,5),(6,8,10),(8,6,10)]
-}

{- FOURMOLU_DISABLE -}
pyths :: Int -> [(Int, Int, Int)]
pyths n =
    [ (x, y, z) | x <- range
                , y <- range
                , z <- range
                , x * x + y * y == z * z
    ]
  where
    range = [1 .. n]
{- FOURMOLU_ENABLE -}

{- | 6. Find perfect numbers

>>> perfects 500
[6,28,496]
-}

-- From chapter 5, with a slight variation (excludes the number itself)
factors :: Int -> [Int]
factors n = [x | x <- [1 .. n - 1], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1 .. n - 1], sum (factors x) == x]

{- | 7. Show how the list comprehension `[(x,y) | x <- [1,2], y <- [3,4]]` with
2 generators can be re-expressed using two comprehensions with single generators


>>> [(x,y) | x <- [1,2], y <- [3,4]]
[(1,3),(1,4),(2,3),(2,4)]

I got this, but it's not quite the same
>>> concat [[x | x <- [ (1,y), (2,y) ]] | y <- [3,4]]
[(1,3),(2,3),(1,4),(2,4)]

Got it!
>>> concat [[xs | xs <- [ (x,3), (x,4) ]] | x <- [1,2]]
[(1,3),(1,4),(2,3),(2,4)]
-}

{- | 8. Redefine the function `positions` using the function `find`

>>> positions 0 [1,2,0,3,4,0,5,0]
[2,5,7]
-}

-- From Chapter 5
find :: (Eq k) => k -> [(k, v)] -> [v]
find k lst = [v | (k', v) <- lst, k == k']

positions :: (Eq a, Num b, Enum b) => a -> [a] -> [b]
positions n xs = find n (zip xs [0 ..])

{- | 9. The **scalar product** of two lists of integers `xs` and `ys` of length `n`
is given by the sum of the products of corresponding integers:

http://asciimath.org

  sum_(i=0)^(n-1) (xs_i * ys_i)

Define scalarProduct using list comprehensions

>>> scalarProduct [1,2,3] [4,5,6]
32
-}

-- scalarProduct :: (Num a) => [a] -> [a] -> a
scalarProduct :: (Num a) => [a] -> [a] -> a
scalarProduct xs ys = sum [x * y | (x, y) <- zip xs ys]

-- | 10. Modify the Caesar cipher to also handle upper-case letters -> DONE