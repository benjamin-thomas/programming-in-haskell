{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{- HLINT ignore "Eta reduce" -}
{- HLINT ignore "Use foldr" -}
{- HLINT ignore "Redundant bracket" -}

module Chapter14.Lesson2 where

import Prelude
    ( Bool (..)
    , Eq
    , Maybe (..)
    , Num
    , Ord
    , Read
    , Show
    , even
    , flip
    , map
    , undefined
    , ($)
    , (&&)
    , (*)
    , (+)
    , (++)
    , (-)
    , (.)
    , (||)
    )

class Monoid a where
    mempty :: a
    mappend :: a -> a -> a

-- mconcat :: [a] -> a
-- mconcat = foldr mappend mempty

newtype Sum a = Sum a
    deriving
        ( Eq
        , Ord
        , Show
        , Read
        )

newtype Product a = Product a
    deriving
        ( Eq
        , Ord
        , Show
        , Read
        )

instance (Num a) => Monoid (Sum a) where
    mempty = Sum 0
    mappend (Sum a) (Sum b) = Sum (a + b)

instance (Num a) => Monoid (Product a) where
    mempty = Product 1
    mappend (Product a) (Product b) = Product (a * b)

--- Foldables

{-

The main goal of monoids is to reduce values in a data structure to a single value.

In the case of lists, this operation could be fold

λ> foldList (map Sum [2, 3, 4])
Sum 9

λ> foldList (map Product [2, 3, 4])
Product 24

Equivalent to:
λ> Product 2 `mappend` (Product 3 `mappend` (Product 4 `mappend` mempty))
Product 24

Equivalent to:
λ> Product 2 `mappend` Product 3 `mappend` Product 4 `mappend` mempty
Product 24

It's equivalent to `mconcat` which uses `foldr` under the hood (see line 45).
The difference is that foldList here uses explicit recursion.
λ> mconcat (map Product [2, 3, 4])
Product 24

-}
foldList :: (Monoid a) => [a] -> a
foldList [] = mempty
foldList (x : xs) = x `mappend` foldList xs

{-

In a similar manner, we could define fold on a Tree data structure

λ> foldTree $ Node (Leaf (Product 3)) (Leaf (Product 4))
Product 12

λ> foldTree $ Node (Leaf (Sum 3)) (Leaf (Sum 4))
Sum 7

Huston, we got a problem -> can't use `mconcat` now, since it's tied to the list type:

From our small lib:
λ> :t mconcat
mconcat :: Monoid a => [a] -> a

Or from actual Prelude:
Prelude Data.Monoid> :i mconcat
type Monoid :: * -> Constraint
class Semigroup a => Monoid a where
  ...
  mconcat :: [a] -> a
        -- Defined in ‘GHC.Base’

-}
data Tree a
    = Leaf a
    | Node (Tree a) (Tree a)
    deriving (Show)

foldTree :: (Monoid a) => Tree a -> a
foldTree (Leaf a) = a
foldTree (Node l r) = foldTree l `mappend` foldTree r

{-

So, because `mconcat` is not generalized to other data structures, we define a Foldable class instead

We don't need to require a Monoid instance for `foldr` and `foldl` since the
starting value + mapping function already convey the notion of combining.

-}
class Foldable t where
    fold :: (Monoid a) => t a -> a
    foldMap :: (Monoid b) => (a -> b) -> t a -> b
    foldr :: (a -> b -> b) -> b -> t a -> b
    foldl :: (b -> a -> b) -> b -> t a -> b

{-

>>> fold $ map Sum [1,2,3]
Sum 6

>>> foldMap Sum [1,2,3]
Sum 6

>>> foldr (\new acc -> new - acc) 0 [1,2,3]
2

Equivalent to:
1 - (2 - (3 - 0))

>>> foldl (\acc new -> acc - new) 0 [1,2,3]
-6

Equivalent to:
((0 - 1) - 2) - 3

-}
instance Foldable [] where
    fold :: (Monoid a) => [a] -> a
    fold [] = mempty
    fold (x : xs) = x `mappend` fold xs

    foldMap :: (Monoid b) => (a -> b) -> [a] -> b
    foldMap _ [] = mempty
    foldMap f (x : xs) = f x `mappend` foldMap f xs

    foldr :: (a -> b -> b) -> b -> [a] -> b
    foldr _ seed [] = seed
    foldr f seed (x : xs) = f x (foldr f seed xs)

    -- My version below:
    -- foldl :: (b -> a -> b) -> b -> [a] -> b
    -- foldl f seed lst =
    --     aux seed lst
    --   where
    --     aux acc []     = acc
    --     aux acc (x:xs) = aux (f acc x) xs

    -- The book's version below
    foldl :: (b -> a -> b) -> b -> [a] -> b
    foldl _ acc [] = acc
    foldl f acc (x : xs) = foldl f (f acc x) xs

{-

>>> fold $ Node (Leaf (Sum 3)) (Leaf (Sum 4))
Sum 7

>>> foldMap (\(Sum x) -> Sum $ x + 1) $ Node (Leaf (Sum 3)) (Leaf (Sum 4))
Sum 9

>>> foldMap Product $ Node (Leaf 3) (Leaf 4)
Product 12

>>> foldl (-) 0 $ Node (Leaf 3) (Leaf 4)
-7
>>> foldr (-) 0 $ Node (Leaf 3) (Leaf 4)
1

-}
instance Foldable Tree where
    fold :: (Monoid a) => Tree a -> a
    fold (Leaf a) = a
    fold (Node l r) = fold l `mappend` fold r

    foldMap :: (Monoid b) => (a -> b) -> Tree a -> b
    foldMap f (Leaf a) = f a
    foldMap f (Node l r) = foldMap f l `mappend` foldMap f r

    foldr :: (a -> b -> b) -> b -> Tree a -> b
    foldr f b (Leaf a) = f a b
    foldr f b (Node l r) = foldr f (foldr f b l) r

    foldl :: (b -> a -> b) -> b -> Tree a -> b
    foldl f b (Leaf a) = f b a
    foldl f b (Node l r) = foldl f (foldl f b r) l
