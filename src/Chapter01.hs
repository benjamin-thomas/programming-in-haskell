{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
module Chapter01 where

hello :: [Char]
hello = "World!"

{-
cabal exec ghci src/Chapter01.hs

Or

cabal repl
> Chapter01.hello -- optionally import Chapter01 to jump into its namespace (no module prefix)

Or

ghcid --command "cabal repl" --test "Chapter01.hello"
ghcid -c "cabal repl" -T "Chapter01.hello"

cabal repl --with-ghc=doctest
cabal exec doctest src/

find ./src/ | entr -rc cabal exec doctest src/
ghcid --command 'cabal new-repl' --test ':!doctest ./src/Chapter01.hs'

-}

{- | In functional programming, the function parameters get replaced inside the function's body

double x = x + x
double 3 = 3 + 3
double 3 = 6

>>> double 3
6


Evaluating the left inner function first is shorter (compute the function's parameter first):

double x = x + x
double (double 3) = double (3 + 3)
double (double 3) = double 6
double (double 3) = 6 + 6
double (double 3) = 12


Evaluating first the outer function is longer (replace `x` param with `double 3` on the right):

double x = x + x
double (double 3) = (double 3) + (double 3)
double (double 3) = (3 + 3) + (double 3)
double (double 3) = (3 + 3) + (double 3)
double (double 3) = (6) + (double 3)
double (double 3) = (6) + (3 + 3)
double (double 3) = (6) + (6)
double (double 3) = (6) + (6)
double (double 3) = 12

>>> double (double 3)
12
-}

double :: Num a => a -> a
double x = x + x

{- | Another demonstration of parameter substitution
sum [1..5] = sum [1, 2, 3, 4, 5] -- apply `[..]`
sum [1..5] = 1 + 2 + 3 + 4 + 5   -- apply `sum`
sum [1..5] = 15                  -- apply `(+)`
>>> sum [1..5]
15
-}

{- | define our own sum function

>>> sum' [1,2,3]
6

sum' [1, 2, 3] = 1 + sum' [2, 3]
sum' [1, 2, 3] = 1 + 2 + sum' [3]
sum' [1, 2, 3] = 1 + 2 + 3 + sum' []
sum' [1, 2, 3] = 1 + 2 + 3 + 0
sum' [1, 2, 3] = 1 + 2 + 3
sum' [1, 2, 3] = 1 + 5
sum' [1, 2, 3] = 6


The type signature `Num a => [a] -> a` means:
For any number of type `a`, transform a list of `a`s to an `a`.
 -}
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (n:ns) = n + sum' ns
