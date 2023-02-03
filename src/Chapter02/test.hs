{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use last" #-}
module Test where

{- Such simple script files can be loaded straight into GHCi, as such:

    ghci ./src/Chapter02/test.hs

    To reload the REPL after file save, run:
    :reload

    Or its shortcut:
    :r
 -}

double x = x + x
quadruple x = double x + double x

firstFour = take (double 2) [1 ..]

factorial n = product [1 .. n]

average ns = sum ns `div` length ns

-- Spacing itself removes any ambiguities
a = b + c
  where
    b = 1
    c = 2
d = a * 2

-- Additional curly brackets may be used
-- a' = b + c where {b = 1; c = 2}; d' = a * 2

-- QUESTIONS

{- 1. Work through the examples of this chapter using GHCi

      Already done
 -}

{- 2. Parenthesize the following numeric expressions

      [ 2^3*4    ,    (2^3)*4]
      [ 2*3+4*5  ,    (2*3)+(4*5)]
      [ 2+3*4^5  ,    2+(3*(4^5))]
 -}

{- 3. The script below contains three syntactic errors.

      Correct them, then check in GHCi

      N = a 'div' length xs
          where
              a = 10
             xs = [1,2,3,4,5]

:{
n = a `div` length xs
    where
        a  = 10
        xs = [1,2,3,4,5]
:}

 -}

{- 4. The library function `last` selects the last element of a non-empty list; for
      example, `last [1,2,3,4,5] = 5.

      Show how the function `last` could be defined in terms of the other library functions
      introduced in this chapter.

      Can you think of another possible definition?
 -}

{- |

>>> last' [1,2,3]
3

>>> last'' [1,2,3]
3

>>> last''' [1,2,3]
3
-}
last' ns = ns !! (length ns - 1)

last'' [] = error "Invalid argument: empty list"
last'' [n] = n
last'' (n : ns) = last'' ns

-- Book's solution
last''' xs = head (reverse xs)

{- 5. The library function `init` removes the last element from a non-empty list; for
      example, `init [1,2,3,4,5] = [1,2,3,4].

      Show how `init` could similarly be defined in two different ways.
 -}

{- |
>>> init' [1..5]
[1,2,3,4]
>>> init'' [1..5]
[1,2,3,4]
-}
init' [] = error "Invalid argument: empty list"
init' [x] = []
init' (n : ns) = n : init' ns

init'' ns = take (length ns - 1) ns
