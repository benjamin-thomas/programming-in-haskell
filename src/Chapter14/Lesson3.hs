{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Chapter14.Lesson3 where

import Data.Foldable
    ( fold
    , toList
    )

import Data.Monoid

data Tree a
    = Leaf a
    | Node (Tree a) (Tree a)
    deriving (Show)

instance Foldable Tree where
    foldMap :: (Monoid b) => (a -> b) -> Tree a -> b
    foldMap f (Leaf a) = f a
    foldMap f (Node l r) = foldMap f l `mappend` foldMap f r

tree :: Tree Int
tree =
    Node
        ( Node
            (Leaf 1)
            (Leaf 2)
        )
        (Leaf 3)

{-

We get a bunch of useful functions for free

>>> null tree
False

>>> length tree
3

"Normal"
>>> foldr (+) 0 tree
6

"For free"
>>> foldr1 (+) tree
6

>>> elem 3 tree
True
>>> elem 4 tree
False

>>> minimum tree
1

>>> maximum tree
3

>>> sum tree
6

>>> product tree
6

>>> toList tree
[1,2,3]

-}

-- Note that:
-- null tree = null $ toList tree
-- The equivalence applies to many functions (convert toList)
-- This means that default definitions for Foldable delegate to `toList`

--------------------------------------
-- Fold is defined in terms of foldMap
--------------------------------------
{- fold = foldMap id

So `fold` is defined in terms of `foldMap`

>>> foldMap id [[1], [2,3]]
[1,2,3]

>>> foldMap (map (*2)) [[1], [2,3]]
[2,4,6]

>>> fold [[1], [2,3]]
[1,2,3]

>>> fold ["Hell", "o"]
"Hello"

>>> foldMap id ["AB", "C"]
"ABC"

>>> foldMap (map succ) ["AB", "C"]
"BCD"
-}

---------------------------------------
-- foldMap is defined in terms of foldr
---------------------------------------
{- foldMap f = foldr (mappend . f) mempty

>>> foldMap id [[1], [2,3]]
[1,2,3]

>>> foldr (mappend . id) mempty [[1], [2,3]]
[1,2,3]

>>> foldMap (map (*2)) [[1], [2,3]]
[2,4,6]

>>> foldr (mappend . map (*2)) mempty [[1], [2,3]]
[2,4,6]

>>> foldMap id ["AB", "C"]
"ABC"

>>> foldr (mappend . id) mempty ["AB", "C"]
"ABC"

>>> foldMap (map succ) ["AB", "C"]
"BCD"

>>> foldr (mappend . map succ) mempty ["AB", "C"]
"BCD"

 -}

----------------------------------------
-- toList is defined in terms of foldMap
----------------------------------------
{- toList = foldMap (\x -> [x])

>>> toList tree
[1,2,3]

>>> foldMap (\x -> [x]) tree
[1,2,3]

-}

{-

CONCLUSION: this is why we can define Foldable only in terms of foldMap!

-}

{-

So `fold` can be seen like `mconcat`, but more "general":

>>> mconcat [[1, 2, 3], [4, 5], [6], []]
[1,2,3,4,5,6]
>>> fold [[1, 2, 3], [4, 5], [6], []]
[1,2,3,4,5,6]

>> mconcat $ Node (Node (Leaf [1]) (Leaf [2,3])) (Leaf [4,5,6])
NOT POSSIBLE: mconcat works on Monoid, not Foldable

>>> fold $ Node (Node (Leaf [1]) (Leaf [2,3])) (Leaf [4,5,6])
[1,2,3,4,5,6]

 -}

{-

Regarding foldMap...

These is equivalent operations:

>>> foldMap Sum [1,2,3]
Sum {getSum = 6}

>>> Sum 1 <> Sum 2 <> Sum 3
Sum {getSum = 6}

---

Now, let's talk about this operation:

>>> foldMap Sum tree
Sum {getSum = 6}

Due to the definition for foldMap on Tree, it gets transformed like such:

>>> foldMap Sum (Node (Node (Leaf 1) (Leaf 2)) (Leaf 3))
Sum {getSum = 6}
>>> foldMap Sum (Node (Leaf 1) (Leaf 2)) `mappend` foldMap Sum (Leaf 3)
Sum {getSum = 6}

>>> (foldMap Sum (Leaf 1) `mappend` foldMap Sum (Leaf 2)) `mappend` foldMap Sum (Leaf 3)
Sum {getSum = 6}

Now, according to the foldMap definition, we "pop off" the leaves (with the accompanying foldMap func)
>>> (Sum 1 `mappend` Sum 2) `mappend` Sum 3
Sum {getSum = 6}

Simplifies to:
>>> Sum 1 <> Sum 2 <> Sum 3
Sum {getSum = 6}

 -}

-- GENERIC FUNCTIONS

{-

In Chapter02/test.hs, we defined average, like such:

average ns = sum ns `div` length ns

Now we can make a more "general" average function, that works with any Foldable (List, our Tree, etc.)

>>> average [1..10]
5

>>> average (Node (Leaf 1) (Leaf 3))
2
 -}
average :: (Foldable t) => t Int -> Int
average ns = sum ns `div` length ns

{-

We also get a bunch of functions "for free", such as

all p = getAll . foldMap (All . p)

>>> all odd (Node (Leaf 1) (Leaf 3))
True

Same as (without the getAll extraction):

>>> foldMap (All . odd) (Node (Leaf 1) (Leaf 3))
All {getAll = True}

Now we "expand" foldMap as previously

>>> foldMap (All . odd) (Leaf 1) `mappend` foldMap (All . odd) (Leaf 3)
All {getAll = True}

Now we can "pop off" the leaves
>>> (All . odd) 1 `mappend` (All . odd) 3
All {getAll = True}

-- Simplified to:
>>> (All . odd) 1 <> (All . odd) 3
All {getAll = True}

Now, since

(f . g) x = f (g x)

These are equivalent:

>>> (All . odd) 1
All {getAll = True}

>>> All (odd 1)
All {getAll = True}

So...

>>> All (odd 1) <> All (odd 3)
All {getAll = True}
 -}

{-

We get other similar functions "for free":

---
>>> and (Node (Leaf True) (Leaf True))
True

>>> and (Node (Leaf True) (Leaf False))
False

---

>>> or (Node (Leaf True) (Leaf True))
True

>>> or (Node (Leaf True) (Leaf False))
True

---

>>> any odd (Node (Leaf 2) (Leaf 3))
True

>>> any odd (Node (Leaf 2) (Leaf 4))
False

 -}
