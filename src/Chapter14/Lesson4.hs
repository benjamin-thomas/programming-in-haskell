{- HLINT ignore "Use <$>" -}
{-# LANGUAGE InstanceSigs #-}

module Chapter14.Lesson4 where

{-

cabal repl --build-depends pretty-simple
> :m Text.Pretty.Simple Chapter14.Lesson4

 -}

-- Traversable

{-

Reminder: mapping over a list

>>> map_ (*2) [1..3]
[2,4,6]

 -}
{- HLINT ignore "Use map" -}
{- HLINT ignore "Use foldr" -}
map_ :: (a -> b) -> [a] -> [b]
map_ _ [] = []
map_ f (x : xs) = f x : map_ f xs

{-

Rather then applying (a -> b), suppose we would rather apply (a -> Maybe b) to signal failure.

We could call such a function "traverse".

Because Maybe is Applicative:

>>> pure (*) <*> Just 3 <*> Just 4
Just 12

>>> (*) <$> Just 3 <*> Just 4
Just 12

We can define traverse_ as such:

>>> traverse_ (\x -> if x <= 0 then Nothing else Just x) [1,2,3]
Just [1,2,3]

>>> traverse_ (\x -> if x <= 0 then Nothing else Just x) [1,-2,3]
Nothing

In summary, "traverse_" here provides a simple means of traversing the elements
of a list using a function that may fail.
 -}
traverse_ :: (a -> Maybe b) -> [a] -> Maybe [b]
traverse_ _ [] = Just []
-- traverse_ f (x : xs) = (:) <$> f x <*> traverse_ f xs
traverse_ f (x : xs) = pure (:) <*> f x <*> traverse_ f xs

{-

The idea of traversing a data structure isn't specific to lists, and isn't
specific to functions that may fail.

Tha class of types that support such a generalized mapping are known as
"traversable types", or "traversables" for short.

In Haskell this concept is captured by the following built-in class declaration:

class (Functor t, Foldable t) => Traversable t where
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

---

The traverse default definition can be explained like such:

1) First we apply "g"
>>> fmap Just tree
Node (Node (Leaf (Just 1)) (Leaf (Just 2))) (Leaf (Just 3))

2) Then we combine sequenceA which "turns the structure inside-out"
>>> (sequenceA' . fmap Just) tree
Just (Node (Node (Leaf 1) (Leaf 2)) (Leaf 3))

Equivalent to:
>>> traverse' Just tree
Just (Node (Node (Leaf 1) (Leaf 2)) (Leaf 3))
 -}

class (Functor t, Foldable t) => Traversable' t where
    traverse' :: (Applicative f) => (a -> f b) -> t a -> f (t b)
    traverse' g = sequenceA' . fmap g -- default definition, 🤯️ (book says it's preferable to define traverse when declaring an instance). See above for a breakdown....

    sequenceA' :: (Applicative f) => t (f a) -> f (t a) -- See above `Tree` definition
    sequenceA' = traverse' id

    -- Default definitions for the special cases where the effects are monadic, rather than applicative
    mapM' :: (Monad m) => (a -> m b) -> t a -> m (t b)
    mapM' = traverse'

    sequence' :: (Monad m) => t (m a) -> m (t a)
    sequence' = sequenceA'

{-

Because lists are functorial and foldable, we can make it traversable by
generalizing the Maybe type from the example above.

 -}
instance Traversable' [] where
    traverse' :: (Applicative f) => (a -> f b) -> [a] -> f [b]
    traverse' _ [] = pure []
    traverse' f (x : xs) = pure (:) <*> f x <*> traverse' f xs

data Tree a
    = Leaf a
    | Node (Tree a) (Tree a)
    deriving (Show)

instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Node l r) = Node (fmap f l) (fmap f r)

{-
instance Foldable Tree where
    foldr :: (a -> b -> b) -> b -> Tree a -> b
    foldr f b (Leaf a) = f a b
    foldr f b (Node l r) = foldr f (foldr f b r) l
-}

instance Foldable Tree where
    foldMap :: (Monoid m) => (a -> m) -> Tree a -> m
    foldMap f (Leaf x) = f x
    foldMap f (Node l r) = foldMap f l <> foldMap f r

{-

Now that we defined Foldable, we've got foldr

>>> foldr (+) 0 tree
6

 -}

instance Traversable' Tree where
    traverse' :: (Applicative f) => (a -> f b) -> Tree a -> f (Tree b)
    traverse' f (Leaf x) = Leaf <$> f x
    traverse' f (Node l r) = Node <$> traverse' f l <*> traverse' f r

{-

Now that we've defined Traversable, we can "traverse".

>>> traverse' (\x -> if x == 0 then Nothing else Just x) tree
Just (Node (Node (Leaf 1) (Leaf 2)) (Leaf 3))

>>> traverse' (\x -> if x == 2 then Nothing else Just x) tree
Nothing

---

The Traversable class also provides `sequenceA` with a default definition

>>> sequenceA' [Just 1, Just 2, Just 3]
Just [1,2,3]

>>> sequenceA' [Just 1, Nothing, Just 3]
Nothing

>>> sequenceA' (Node (Leaf $ Just 1) (Node (Leaf $ Just 2) (Leaf $ Just 3)))
Just (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))

>>> sequenceA' (Node (Leaf $ Just 1) (Node (Leaf Nothing) (Leaf $ Just 3)))
Nothing

 -}

tree :: Tree Int
tree =
    Node
        ( Node
            (Leaf 1)
            (Leaf 2)
        )
        (Leaf 3)
