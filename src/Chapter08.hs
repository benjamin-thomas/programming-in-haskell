{-# HLINT ignore "Use foldl" #-}
{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Use foldr" #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Chapter08 where

import Data.Function ((&))
import Prelude hiding (Bool (..), Maybe (..), String)

-- A type synonym. A new type must start with a capital letter.
type String = [Char]

-- Type declarations (aliases) don't allow recursion.

-- This is allowed
type Position = (Int, Int)
type Transform = Position -> Position

-- This is not allowed. We will need to use the more powerful "data" declaration.
-- type Tree a = (Int, [Tree])

-- We can parameterize the type declaration with other types
type Pair a = (a, a)

-- We can use more than one type parameter
type Assoc k v = [(k, v)]

-- >>> table = [(1, "one"), (2, "two"), (3, "three")]
-- >>> find 2 table
-- "two"

-- find :: (Eq k) => k -> [(k, v)] -> v
find :: (Eq k) => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

-- Not valid syntax
-- type Bool = False | True

-- A completely new type can be created with `data`
data Bool = True | False

-- We can pass around the type parameter to functions
data Direction = North | East | South | West

{- FOURMOLU_DISABLE -}
move :: Direction -> Position -> Position
move North (x, y) = (  x, y+1)
move  East (x, y) = (x+1,   y)
move South (x, y) = (  x, y-1)
move  West (x, y) = (x-1,   y)
{- FOURMOLU_ENABLE -}

{- |
>>> moves [North, North] (1,0)
(1,2)
-}
moves :: [Direction] -> Position -> Position
moves [] pos = pos
moves (x : xs) pos = moves xs (move x pos) -- foldl

{- |
>>> moves' [North, North] (1,0)
(1,2)
-}
moves' :: [Direction] -> Position -> Position
moves' [] pos = pos
moves' (x : xs) pos = move x (moves' xs pos) -- foldr

rev :: Direction -> Direction
rev North = South
rev South = North
rev West = East
rev East = West

rev2 :: Direction -> Direction
rev2 dir = case dir of
  North -> South
  South -> North
  East -> West
  West -> East

rev3 :: Direction -> Direction
rev3 = \case
  North -> South
  South -> North
  West -> East
  East -> West

-- The constructors in data declarations can also have arguments
data Shape
  = Circle Float -- radius
  | Rect Float Float -- width height

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Rect w h) = w * h
area (Circle r) = pi * r ^ 2

-- Data declarations themselves can be parameterized
data Maybe a = Nothing | Just a

safeDiv :: (Integral a) => a -> a -> Maybe a
safeDiv _ 0 = Nothing
safeDiv m n = Just (m `div` n)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

-- If there is only one variant, then we can use `newtype` instead of `data`
newtype NatSimple = N Int

-- We can also describe a Nat type in a recursive fashion
data Nat
  = Zero
  | Succ Nat
  deriving (Show)

three :: Nat
three = Succ $ Succ $ Succ Zero

four :: Nat
four = Zero & Succ & Succ & Succ & Succ

-- 🤯️
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

-- 🤯️🤯️🤯️
int2nat :: Int -> Nat
int2nat n
  | n == 0 = Zero
  | n > 0 = Succ $ int2nat (n - 1)
  | otherwise = error "Bad input"

{- |

>>> nat2int three
3

>>> nat2int four
4

>>> int2nat 0
Zero

>>> int2nat 1
Succ Zero

>>> int2nat 2
Succ (Succ Zero)

>>> int2nat 3
Succ (Succ (Succ Zero))

>>> int2nat (-1)
*** Exception: Bad input
...

>>> addSimple three Zero
Succ (Succ (Succ Zero))

>>> addSimple Zero four
Succ (Succ (Succ (Succ Zero)))

>>> nat2int $ addSimple three four
7

>>> nat2int $ add three four
7
-}
addSimple :: Nat -> Nat -> Nat
addSimple a b = int2nat (nat2int a + nat2int b)

{- |

>>> nat2int $ add three Zero
3

>>> nat2int $ add Zero four
4

>>> nat2int $ add three four
7

>>> nat2int $ add Zero Zero
0

>>> add Zero (Succ Zero)
Succ Zero

>>> add (Succ Zero) Zero
Succ Zero

>>> add (Succ Zero) (Succ Zero)
Succ (Succ Zero)

>>> add (Succ (Succ Zero)) (Succ Zero)
Succ (Succ (Succ Zero))

>>> add (Succ Zero) (Succ (Succ Zero))
Succ (Succ (Succ Zero))

>>> add (Succ (Succ Zero)) (Succ (Succ Zero))
Succ (Succ (Succ (Succ Zero)))

>>> nat2int $ add' three four
7

>>> nat2int $ add' three Zero
3
-}
add :: Nat -> Nat -> Nat
add a Zero = a
add Zero b = b
add (Succ a) (Succ b) = Succ (Succ (add a b)) -- this is like saying "add 2 while popping off 2 values"
{-

add (Succ (Succ Zero)) (Succ Zero)
= { applying add }
  Succ (Succ (add (Succ Zero) Zero))
= { applying add }
  Succ (Succ (Succ Zero))
 -}

-- Now for the book's definition. It's "better" because we don't have to consume `b`.
-- Instead, we can just insert it into our 'function stack" once we've fully consumed `a`.
{-

add (Succ (Succ Zero)) (Succ Zero)
= { applying add }
  Succ (add (Succ Zero) (Succ Zero))
= { applying add }
  Succ (Succ (add Zero (Succ Zero)))
= { applying add }
  Succ (Succ (Succ Zero))

 -}
add' :: Nat -> Nat -> Nat
add' Zero b = b
add' (Succ a) b = Succ (add' a b)

{-

Another example of the data mechanism is defining our own List type

 -}

data List a
  = Nil
  | Cons a (List a)
  deriving (Show)

{- |

>>> len Nil
0

>>> len (Cons 1 (Cons 2 (Cons 3 Nil)))
3
-}
len :: List a -> Int
len Nil = 0
len (Cons _ xs) = 1 + len xs

{- | A non-empty list
>>> len' (End 0)
1

>>> len' (Cons' 1 (End 2))
2

>>> len' (Cons' 1 (Cons' 2 (End 3)))
3
-}
data List' a
  = End a
  | Cons' a (List' a)
  deriving (Show)

len' :: List' a -> Int
len' (End _) = 1
len' (Cons' _ xs) = 1 + len' xs
