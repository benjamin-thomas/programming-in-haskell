{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- HLINT ignore "Use <$>" -}

{- HLINT ignore "Use sequenceA" -}

{- HLINT ignore "Use traverse" -}

module Chapter14.Exercises where

import Data.List ((++))
import Data.String
import Prelude
    ( Applicative
    , Functor
    , Show
    , fmap
    , id
    , length
    , pure
    , undefined
    , (+)
    , (.)
    , (<*>)
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

instance Semigroup String where
    (<>) :: String -> String -> String
    (<>) = (++)

instance Monoid String where
    mempty :: String
    mempty = ""

    mappend :: String -> String -> String
    mappend = (<>)

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
