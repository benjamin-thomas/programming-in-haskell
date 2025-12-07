{-# OPTIONS_GHC
    -Wall
    -Wno-type-defaults
#-}

{-# LANGUAGE NoImplicitPrelude #-}

import Prelude
    ( Show
    , Num
    , Eq
    , Int
    , Bool(True,False)
    , Char
    , String
    , IO
    , return
    , concat
    , concatMap
    , even
    , id
    , getChar
    , getLine
    , length
    , not
    , read
    , replicate
    , (==)
    , (.)
    , ($)
    , (+)
    , (-)
    , (*)
    , (^)
    , (>>=)
    )

-- Notice the same "shape"
inc :: [Int] -> [Int]
inc []     = []
inc (n:ns) = n+1 : inc ns

sqr :: [Int] -> [Int]
sqr []     = []
sqr (n:ns) = n^2 : sqr ns

-- This is "map"
map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs


-- Now we can define our "inc" function more succintly
inc' :: Num n => [n] -> [n]
inc' = map (+1)

sqr' :: Num n => [n] -> [n]
sqr' = map (^2)


{-

This idea of mapping a function over each elements isn't tied to the List type.

Instead, a type supporting such mapping would be known as a `Functor`.

-}


-- Definition
class Functor f where
    fmap :: (a -> b) -> f a -> f b


-- List type implementation
instance Functor [] where
    fmap :: (a -> b) -> [a] -> [b]
    fmap = map


data Maybe a
    = Nothing
    | Just a
    deriving (Show, Eq)

instance Functor Maybe where
    fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap _ Nothing   = Nothing
    fmap f (Just a)  = Just (f a)


-- User defined types

data Tree a
    = Leaf a
    | Node (Tree a) (Tree a)
    deriving Show


instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap f (Leaf a)   = Leaf (f a)
    fmap f (Node x y) = Node (fmap f x) (fmap f y)


instance Functor IO where
    fmap :: (a -> b) -> IO a -> IO b
    fmap f ma = do {a <- ma; return (f a)}


-- Re-implement `inc`, generalized to functors
inc'' :: Functor f => Num a => f a -> f a
inc'' = fmap (+1)


-- APPLICATIVES

{-

Functors apply single argument functions (think `fmap succ [1,2,3]`).

Applicative functors, instead, can apply functions of many more arguments.

This idea is similar to map2, map3, map4... mapN in Elm.

-}


class Functor2 f where
    fmap2 :: (a -> b -> c) -> f a -> f b -> f c

class Functor3 f where
    fmap3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d

{-

Now we can do that:

*Main> fmap2 (+) (Just 1) (Just 2)
Just 3

Just like in Elm:

> Maybe.map2 (+) (Just 2) (Just 3)
Just 5 : Maybe number

However, the downside is that this would multiply the instance declarations.

And it's not very elegant, there's a lot of repetitive code...

-}
instance Functor2 Maybe where
    fmap2 _f  Nothing  _b      = Nothing
    fmap2 _f  _a       Nothing = Nothing
    fmap2  f (Just a) (Just b) = Just (f a b)


{-

Haskell
=======

*Main> fmap3 (\a b c -> a*b + c) (Just 3) (Just 4) (Just 10)
Just 22

---

Elm
===

> Maybe.map3 (\a b c -> a*b + c) (Just 3) (Just 4) (Just 10)
Just 22 : Maybe number

-}
instance Functor3 Maybe where
    fmap3 _f  Nothing  _b       _c      = Nothing
    fmap3 _f  _a       Nothing  _c      = Nothing
    fmap3 _f  _a       _b       Nothing = Nothing
    fmap3  f (Just a) (Just b) (Just c) = Just (f a b c)


{-

"Applicative style", instead, uses the idea of currying and function
application to "abstract away" the applied function (`(a -> b)`).

The style is said to be "applicative" because it is similar to "normal"
function application proposed by currying, whereas any multi-arg function is,
really, a single arg function that produces another function.

-}


class Functor f => Applicative f where
    pure  :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b


-- Because `Maybe` is already a `Functor`, it's straight-forward to implement `Applicative`.
instance Applicative Maybe where
    pure :: a -> Maybe a
    pure x = Just x

{-
    Nothing <*> _v      = Nothing
    _f      <*> Nothing = Nothing
    Just f  <*> Just v  = Just $ f v
-}
    (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
    Nothing <*> _fa  = Nothing
    Just f  <*>  fa  = fmap f fa

{-

Prelude> pure (+) <*> Just 3 <*> Just 4
Just 7

Prelude> pure (+) <*> [3] <*> [4]
[7]

---

Prelude> pure (+) <*> Nothing <*> Just 4
Nothing

== This is why we viewed empty list as a failure (in earlier chapters)!
Prelude> pure (+) <*> [] <*> [4]
[]

--

Prelude> pure (+) <*> [3,8] <*> [4,2]
[7,5,12,10]

-}


instance Applicative [] where
    pure :: a -> [a]
    pure x = [x]

    --(<*>) fs xs = concatMap (\f -> fmap f xs) fs
    (<*>) :: [(a -> b)] -> [a] -> [b]
    (<*>) fs xs = [f x | f <- fs, x <- xs]


{-
*Main> prods1 [1,2] [3,4]
[3,4,6,8]

*Main> prods2 [1,2] [3,4]
[3,4,6,8]
-}
prods1 :: [Int] -> [Int] -> [Int]
prods1 xs ys = [x*y | x <- xs, y <- ys]

prods2 :: [Int] -> [Int] -> [Int]
prods2 xs ys = pure (*) <*> xs <*> ys


instance Applicative IO where
    pure :: a -> IO a
    pure x = return x

    (<*>) :: IO (a -> b) -> IO a -> IO b
    (<*>) mf ma =
        mf >>= \f ->
        ma >>= \a ->
        return (f a)

    -- Book version
    --mg <*> mx = do {g <- mg; x <- mx; return (g x)}


{-
The applicative style for IO supports a form of *interactive* programming in
which we can apply pure functions to impure arguments without the need to
manage the sequencing of actions or the extraction of result values.
-}
getChars :: Int -> IO String
getChars 0 = return []
getChars n = return (:) <*> getChar <*> getChars (n-1)


{-| Transforms a list of applicative actions into a single action that returns
a list of result values
-}
sequenceA :: Applicative f => [f a] -> f [a]
sequenceA []     = pure []
sequenceA (x:xs) = pure (:) <*> x <*> sequenceA xs


{-

*Main> x <- getChars' 3
*Main> y <- sequenceA [getChar, getChar, getChar]

*Main> (x,y)
("abc","def")

-}
getChars' :: Int -> IO String
getChars' n = sequenceA (replicate n getChar)


{- Applicative has 4 laws

Law 1: Identity — pure id <*> x = x
-----------------------------------
Prelude> (pure id <*> pure 1) == (pure 1 :: Maybe Int)
True

Prelude> pure id <*> pure 1
1
Prelude> pure 1
1



Law 2: Homomorphism — pure (g x) = pure g <*> pure x
----------------------------------------------------
Prelude> pure ((*2) 3) == (pure (*2) <*> pure 3 :: Maybe Int)
True

Prelude> pure ((*2) 3)
6
Prelude> pure (*2) <*> pure 3
6



Law 3: Interchange — u <*> pure y = pure ($ y) <*> u
----------------------------------------------------
Prelude> (pure (+1) <*> pure 5) == (pure ($ 5) <*> pure (+1) :: Maybe Int)
True

Prelude> pure (+1) <*> pure 5
6
Prelude> pure ($ 5) <*> pure (+1)
6


Side note:
Prelude> ($ 5) (+1)
6
Prelude> (\f -> f 5) (+1)
6



Law 4: Composition — pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-----------------------------------------------------------------
Prelude> (pure (.) <*> pure (+1) <*> pure (*2) <*> pure 3) == (pure (+1) <*> (pure (*2) <*> pure 3) :: Maybe Int)
True

Prelude> (pure (.) <*> pure (+1) <*> pure (*2) <*> pure 3)
7
Prelude> pure (+1) <*> (pure (*2) <*> pure 3)
7

-}
