{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use tuple-section" #-}

module Chapter12.Exercises where

import Prelude (
    Char,
    Int,
    Show,
    error,
    id,
    map,
    pred,
    show,
    succ,
    undefined,
    ($),
    (*),
    (+),
    (++),
    (.),
 )

class Functor f where
    fmap :: (a -> b) -> f a -> f b

{-

1) Define an instance of the Functor class for Tree

λ> tree
Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

λ> fmap (*2) tree
Node (Node Leaf 2 Leaf) 4 (Node Leaf 6 Leaf)

λ> fmap even tree
Node (Node Leaf False Leaf) True (Node Leaf False Leaf)

λ> fmap (even . (*2)) tree
Node (Node Leaf True Leaf) True (Node Leaf True Leaf)

λ> fmap even $ fmap (*2) tree
Node (Node Leaf True Leaf) True (Node Leaf True Leaf)

-}
data Tree a
    = Leaf
    | Node (Tree a) a (Tree a)
    deriving (Show)

instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap _ Leaf = Leaf
    fmap f (Node l x r) =
        Node
            (fmap f l)
            (f x)
            (fmap f r)

tree :: Tree Int
tree =
    Node
        (Node Leaf 1 Leaf)
        2
        (Node Leaf 3 Leaf)

{-

2) Complete the instance declaration

So... whereas Maybe requires *one* other type to produce a final type:

Prelude> :kind Maybe
Maybe :: * -> *

(->), the function type constructor, requires 2 types to produce a final type:

Prelude> :kind (->)
(->) :: * -> * -> *

---

We can "compose" unary functions with fmap:

Prelude> let shout = fmap (++ "!") show in shout 1
"1!"

Prelude> let shout = fmap (++ "!") show in shout True
"True!"

fmap for functions is composition:

Prelude> (pred . succ) 'a'
'a'
Prelude> (fmap pred succ) 'a'
'a'

Prelude> ((++ "!") . show) 1
"1!"
Prelude> ((++ "!") . show) (Just 2)
"Just 2!"

-}

instance Functor ((->) a) where
    -- fmap :: (a -> b) -> f a -> f b
    fmap :: (b -> c) -> (->) a b -> (->) a c
    -- fmap :: (b -> c) -> (a -> b) -> (a -> c)
    fmap f g = f . g

{-

3) Define an instance of the applicative class for the type (a ->)

-}
class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

{-

Prelude> (fmap pred succ) 'a'
'a'
Prelude> (pure pred <*> succ) 'a'
'a'
Prelude> (pure show <*> succ) 1
"2"
Prelude> ((<*>) (\_ -> show) succ) 1
"2"

-}
const :: a -> b -> a
const x _ = x

instance Applicative ((->) a) where
    pure :: b -> a -> b
    pure = const

    (<*>) :: (a -> b -> c) -> (a -> b) -> (a -> c)
    (<*>) f g = \a -> f a (g a)

{-

4) Complete the ZipList implementation (normally accessible via Control.Applicative)

λ> fmap (*2) (Z [1,2,3])
Z [2,4,6]

λ> pure (,) <*> Z [1,2,3] <*> Z [2,3,4]
Z [(1,2),(2,3),(3,4)]

-}
newtype ZipList a = Z [a] deriving (Show)

instance Functor ZipList where
    fmap :: (a -> b) -> ZipList a -> ZipList b
    fmap f (Z xs) = Z (map f xs)

repeat :: a -> [a]
repeat x = x : repeat x

{-

λ> map2 (+) [1,2,3] [10,20,30]
[11,22,33]

λ> map2 ($) [(+1), (*2)] [10,20]
[11,40]

λ> map2 (,) [1,2] ['a', 'b']
[(1,'a'),(2,'b')]
-}
map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 _ [] _ = []
map2 _ _ [] = []
map2 f (x : xs) (y : ys) = f x y : map2 f xs ys

instance Applicative ZipList where
    pure :: a -> ZipList a
    pure a = Z (repeat a)

    (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
    (<*>) (Z fs) (Z xs) = Z (map2 ($) fs xs)

{-

5) Work out the types for the variables in the four applicative laws

Law 1: Identity — pure id <*> v = v
  v is `f a`

Law 2: Homomorphism — pure (g v) = pure g <*> pure v
  g is `(a -> b)`
  v is `a`

Law 3: Interchange — u <*> pure y = pure ($ y) <*> u
  u is `f (a -> b)`
  y is `a`

---
btw:
λ> succ 'a'
'b'
λ> ($ 'a') succ
'b'
---

Law 4: Composition — pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
  u is `f (b -> c)
  v is `f (a -> b)
  w is `f a`

-}

{-

6) Define an instance of the Monad class for the type (a ->)

λ> ((*2) >>= (\next prev -> (prev,next))) 1
(1,2)

λ> ((*2) >>= (\next prev -> (prev,next))) 2
(2,4)

λ> ((*2) >>= (\next prev -> (prev,next))) 4
(4,8)

λ> ((*2) >>= (\next prev -> (prev,next))) 8
(8,16)

λ> advance = ((*2) >>= (\next prev -> (prev,next)))
λ> advance 1
(1,2)
λ> advance 2
(2,4)
λ> advance 4
(4,8)
λ> advance 8
(8,16)

-}

class (Applicative m) => Monad m where
    (>>=) :: m a -> (a -> m b) -> m b

instance Monad ((->) a) where
    -- (>>=) ::  m a -> (a -> m b) -> m b
    -- (>>=) :: ((->) a b) -> ((->) b (a -> c)) -> ((->) a c)
    (>>=) :: (a -> b) -> (b -> a -> c) -> a -> c
    (>>=) f g = \a -> g (f a) a

{-

7) Define instances for Functor, Applicative and Monad for Expr.

Also, explain what (>>=) does.

---

So what's `Val` for then?!

λ> fmap (*2) (Add (Val 2) (Var 3))
Add (Val 2) (Var 6)

?!?!
λ> (Add (Var 'a') (Var 'b'))
Add (Var 'a') (Var 'b')

Well, I guess it works!
λ> fmap succ (Add (Var 'a') (Var 'b'))
Add (Var 'b') (Var 'c')

-}

data Expr a
    = Var a
    | Val Int
    | Add (Expr a) (Expr a)
    deriving (Show)

instance Functor Expr where
    fmap :: (a -> b) -> Expr a -> Expr b
    fmap f (Var a) = Var (f a)
    fmap _ (Val n) = Val n -- what does this mean??
    fmap f (Add x y) = Add (fmap f x) (fmap f y)

{-

λ> pure (*) <*> Var 3 <*> Var 4
Var 12

Bizarre!
λ> Add (Var (+1)) (Var (*2)) <*> Var 5
Add (Var 6) (Var 10)

-}
instance Applicative Expr where
    pure :: a -> Expr a
    pure = Var

    (<*>) :: Expr (a -> b) -> Expr a -> Expr b
    (<*>) (Var f) ma = fmap f ma
    (<*>) (Val n) _ = Val n
    (<*>) (Add l r) ma = Add (l <*> ma) (r <*> ma)

{-

λ> Var 3 >>= \n -> pure (succ n)
Var 4

λ> Add (Var 3) (Var 4) >>= \n -> pure (succ n)
Add (Var 4) (Var 5)

λ> Add (Var 1) (Var 2) >>= \x -> Add (Var x) (Var 9)
Add (Add (Var 1) (Var 9)) (Add (Var 2) (Var 9))

 -}
instance Monad Expr where
    (>>=) :: Expr a -> (a -> Expr b) -> Expr b
    (>>=) ea f =
        case ea of
            Var a -> f a
            Val n -> Val n
            Add l r -> Add (l >>= f) (r >>= f)

{-

8) Define the instances of ST for Functor and Applicative in terms of Monad.

-}
type State = Int

newtype ST a = MkST (State -> (a, State))

app :: ST a -> State -> (a, State)
app (MkST f) s = f s

instance Functor ST where
    fmap :: (a -> b) -> ST a -> ST b
    fmap f sta = sta >>= \a -> pure (f a)

{- FOURMOLU_DISABLE -}
instance Applicative ST where
    pure :: a -> ST a
    pure a = MkST (\s -> (a, s))

    (<*>) :: ST (a -> b) -> ST a -> ST b
    stf <*> sta =
        stf >>= \f ->
        sta >>= \a ->
        pure (f a)
{- FOURMOLU_DISABLE -}


{-|

>>> let tick = MkST (\s -> (Data.Char.intToDigit s, s+1))
>>> let threeTicks = tick >>= \a -> tick >>= \b -> tick >>= \c -> pure (a,b,c)
>>> app threeTicks 0
(('0','1','2'),3)


 -}
instance Monad ST where
    (>>=) :: ST a -> (a -> ST b) -> ST b
    st >>= f =
        MkST
            ( \s ->
                let (a, s') = app st s
                 in app (f a) s'
            )
