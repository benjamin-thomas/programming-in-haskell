{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- HLINT ignore "Use list comprehension" -}

{- HLINT ignore "Use id" -}

{- HLINT ignore "Use :" -}

{- HLINT ignore "Use <$>" -}

{- HLINT ignore "Use sequenceA" -}

{- HLINT ignore "Use traverse" -}

module Chapter14.Exercises where

import Data.List (filter, (++))
import Data.String
import Prelude
    ( Applicative
    , Bool (..)
    , Functor
    , Int
    , Show
    , const
    , even
    , flip
    , fmap
    , id
    , not
    , pure
    , undefined
    , ($)
    , (*)
    , (+)
    , (-)
    , (.)
    , (<*>)
    , (>)
    )

{-

Don't use ":m" to load the modules... And don't use pretty-simple either.
":m" tends to bring back chapter 1 (thus prelude), and pretty-simple too it seems.

cabal repl
> :l Chapter14.Exercises

 -}

class Semigroup a where
    (<>) :: a -> a -> a

class (Semigroup a) => Monoid a where
    mempty :: a

    mappend :: a -> a -> a
    mappend = (<>)

-- instance Semigroup String where
--     (<>) :: String -> String -> String
--     (<>) = (++)

-- instance Monoid String where
--     mempty :: String
--     mempty = ""

--     mappend :: String -> String -> String
--     mappend = (<>)

instance (Monoid a, Monoid b) => Semigroup (a, b) where
    (<>) :: (Monoid a, Monoid b) => (a, b) -> (a, b) -> (a, b)
    (<>) (a, b) (c, d) = (a <> c, b <> d)

-- Exercise 1
-- I didn't redefine `mappend`, because that's handled via the Semigroup instance
-- >>> ("a", "b") <> ("c", "d")
-- ("ac","bd")
instance (Monoid a, Monoid b) => Monoid (a, b) where
    mempty :: (Monoid a, Monoid b) => (a, b)
    mempty = (mempty, mempty)

-- Exercise 2: show that the type `a -> b` can be made into a monoid, provided
-- that type `b` is a monoid
-- >>> ((\name -> "Hello, " <> name) <> (\_ -> "!")) "Ben"
-- "Hello, Ben!"
instance (Monoid b) => Semigroup (a -> b) where
    (<>) :: (Monoid b) => (a -> b) -> (a -> b) -> a -> b
    (<>) f g x = f x <> g x

{-

>>> exclaim str = str ++ "!"
>>> exclaim "Ben"
"Ben!"

>>> (exclaim <> mempty) "Ben"
"Ben!"

>>> (mempty <> exclaim) "Ben"
"Ben!"

 -}
instance (Monoid b) => Monoid (a -> b) where
    mempty :: (Monoid b) => (a -> b)
    mempty _ = mempty

{-

3) Show how the `Maybe` type can be made foldable and traversable, by giving
explicit definitions for `fold`, `foldMap`, `foldr`, `foldl`, and `traverse`

 -}

data Maybe a
    = Just a
    | Nothing
    deriving (Show)

class Foldable t where
    fold :: (Monoid a) => t a -> a
    foldMap :: (Monoid b) => (a -> b) -> t a -> b
    foldr :: (a -> b -> b) -> b -> t a -> b
    foldl :: (b -> a -> b) -> b -> t a -> b

    toList :: t a -> [a]
    toList = foldMap (\x -> [x])

instance (Semigroup a) => Semigroup (Maybe a) where
    (<>) :: Maybe a -> Maybe a -> Maybe a
    (<>) ma Nothing = ma
    (<>) Nothing mb = mb
    (<>) (Just a) (Just b) = Just (a <> b)

{-

>>> Nothing <> Just "cd"
Just "cd"

>>> Just "ab" <> Nothing
Just "ab"

>>> Just "ab" <> Just "cd"
Just "abcd"

 -}

instance (Semigroup a) => Monoid (Maybe a) where
    mempty :: (Semigroup a) => Maybe a
    mempty = Nothing

-- >>> mempty :: Maybe String
-- Nothing

instance Foldable Maybe where
    fold :: (Monoid a) => Maybe a -> a
    fold Nothing = mempty
    fold (Just x) = x

    foldMap :: (Monoid b) => (a -> b) -> Maybe a -> b
    foldMap _ Nothing = mempty
    foldMap f (Just x) = f x

    foldr :: (a -> b -> b) -> b -> Maybe a -> b
    foldr _ acc Nothing = acc
    foldr f acc (Just x) = f x acc

    foldl :: (b -> a -> b) -> b -> Maybe a -> b
    foldl _ acc Nothing = acc
    foldl f acc (Just x) = f acc x

{-

>>> fold (Nothing :: Maybe String)
""

>>> fold (Just "abc")
"abc"

>>> Just "ab" <> Just "cd"
Just "abcd"

>>> foldMap (\str -> str ++ "!") Nothing
""

>>> foldMap (\str -> str ++ "!") (Just "ab")
"ab!"

>>> foldr (\str acc -> acc + (length str)) 0 (Just "abc")
3

>>> foldr (\str acc -> acc + (length str)) 0 Nothing
0

>>> foldl (\acc str -> acc+(length str)) 0 (Just "abc")
3

>>> foldl (\acc str -> acc+(length str)) 0 Nothing
0

 -}

class (Functor t, Foldable t) => Traversable t where
    traverse :: (Applicative f) => (a -> f b) -> t a -> f (t b)
    traverse g = sequenceA . fmap g -- default definition, 🤯️ (book says it's preferable to define traverse when declaring an instance). See above for a breakdown....

    sequenceA :: (Applicative f) => t (f a) -> f (t a) -- See above `Tree` definition
    sequenceA = traverse id

instance Functor Maybe where
    fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap _ Nothing = Nothing
    fmap f (Just x) = Just (f x)

instance Traversable Maybe where
    traverse :: (Applicative f) => (a -> f b) -> Maybe a -> f (Maybe b)
    traverse _ Nothing = pure Nothing
    traverse f (Just a) = pure Just <*> f a

{-

>>> traverse (\n -> [n, n + 10]) Nothing
[Nothing]

>>> traverse (\n -> [n, n + 10]) (Just 3)
[Just 3,Just 13]

 -}

{-
Ex 4:

In a similar manner, show how the following type of binary tree with data in
their nodes can be made into a foldable and traversable type.
-}
data Tree a
    = Leaf
    | Node (Tree a) a (Tree a)
    deriving (Show)

{-
instance Semigroup a => Semigroup (Tree a) where
    (<>) :: Tree a -> Tree a -> Tree a
    (<>) (Node l x r) (Node l' x' r') = Node (l <> l') (x <> x') (r <> r')
    (<>) l    Leaf = l
    (<>) Leaf r    = r
-}

-- Traversable depends on Functor and Foldable
-- Functor and Foldable depend on nothing
tree :: Tree Int
tree =
    Node
        ( Node
            Leaf
            1
            Leaf
        )
        2
        ( Node
            Leaf
            3
            Leaf
        )

tree' :: Tree String
tree' =
    Node
        ( Node
            Leaf
            "a"
            Leaf
        )
        "b"
        ( Node
            Leaf
            "c"
            Leaf
        )

instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap _ Leaf = Leaf
    fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

{-

\*Chapter14.Exercises> tree
Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)
\*Chapter14.Exercises> fmap (*2) tree
Node (Node Leaf 2 Leaf) 4 (Node Leaf 6 Leaf)
\*Chapter14.Exercises> fmap (*2) $ fmap (*2) tree
Node (Node Leaf 4 Leaf) 8 (Node Leaf 12 Leaf)
\*Chapter14.Exercises> fmap (*2) $ fmap (*2) $ fmap (*2) tree
Node (Node Leaf 8 Leaf) 16 (Node Leaf 24 Leaf)

-}

instance Foldable Tree where
    fold :: (Monoid a) => Tree a -> a
    fold Leaf = mempty
    -- Can also be defined as `foldMap id`
    fold (Node l x r) = fold l <> x <> fold r

    foldMap :: (Monoid b) => (a -> b) -> Tree a -> b
    foldMap _ Leaf = mempty
    foldMap f (Node l x r) = foldMap f l <> f x <> foldMap f r

    foldr :: (a -> b -> b) -> b -> Tree a -> b
    foldr _ acc Leaf = acc
    foldr f acc (Node l x r) = foldr f (f x (foldr f acc r)) l

    foldl :: (b -> a -> b) -> b -> Tree a -> b
    foldl _ acc Leaf = acc
    foldl f acc (Node l x r) = foldl f (f (foldl f acc l) x) r

mTree :: Tree (Maybe Int)
mTree =
    Node
        ( Node
            Leaf
            (Just 1)
            Leaf
        )
        (Just 2)
        ( Node
            Leaf
            (Just 3)
            Leaf
        )

instance Traversable Tree where
    sequenceA :: (Applicative f) => Tree (f a) -> f (Tree a)
    sequenceA Leaf = pure Leaf
    sequenceA (Node f x r) = pure Node <*> sequenceA f <*> x <*> sequenceA r

instance Applicative Maybe where
    pure :: a -> Maybe a
    pure = Just

    (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
    Just f <*> Just x = Just (f x)
    _ <*> _ = Nothing

{-

\*Chapter14.Exercises> traverse (\x -> if x > 0 then Just x else Nothing) tree
Just (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf))

\*Chapter14.Exercises> sequenceA mTree
Just (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf))

 -}

{-

Ex 5:

Using [foldMap], define a generic version of the higher-order function [filter]
on lists that can be used with any foldable type

 -}

instance Semigroup [a] where
    (<>) :: [a] -> [a] -> [a]
    xs <> ys = xs ++ ys

instance Monoid [a] where
    mempty :: [a]
    mempty = []

-- treeToList :: (Foldable t) => t a -> [a]
-- treeToList = foldMap (\x -> [x])

{-

>>> filterF even tree
[2]

>>> filterF (not . even) tree
[1,3]

 -}

filterF :: (Foldable t) => (a -> Bool) -> t a -> [a]
-- filterF f = filter f . toList
filterF f = foldMap (\x -> if f x then [x] else [])
