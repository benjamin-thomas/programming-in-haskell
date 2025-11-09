{-

stack ghci --ghci-options "-interactive-print=Text.Pretty.Simple.pPrint" --package pretty-simple ./Exercises.hs

-}
module Chapter08.Exercises where

{- 1. Define a function `mult :: Nat -> Nat`, similar to `add`
-}

data Nat = Zero 
         | Succ Nat deriving Show


toInt :: Nat -> Int
toInt Zero = 0
toInt (Succ n) = 1 + toInt n

toNat :: Int -> Nat
toNat 0 = Zero
toNat n = Succ (toNat $ n-1)

add' :: Nat -> Nat -> Nat
add' a b = toNat $ toInt a + toInt b

add ::  Nat -> Nat -> Nat
add    Zero      b  = b
add (Succ a)     b  = Succ (add a b)


mul' a b = toNat $ toInt a * toInt b

{-


ghci> 7*6
42
ghci> 7 + 7*5
42
ghci> 7 + 7 + 7*4
42
ghci> 7 + 7 + 7 + 7*3
42
ghci> 7 + 7 + 7 + 7 + 7*2
42
ghci> 7 + 7 + 7 + 7 + 7 + 7
42

-}
mul :: Nat ->  Nat -> Nat
mul      a    Zero  = Zero
mul      a (Succ b) = add a (mul a b)


{- 2. Redefine `occurs :: Ord a => a -> Tree a -> Bool` to use `compare`.

Why is it more efficient than the original version?

Compare is defined as:
`compare :: Ord a => a -> a -> Ordering`

-}

data MyTree a = MyLeaf 
              | MyNode (MyTree a) a (MyTree a)
            deriving Show


-- My tree type is slightly different, see page 98.
occurs' :: Ord a => a -> MyTree a -> Bool
occurs' x MyLeaf = False
occurs' x (MyNode left y right) = x == y || occurs' x left || occurs' x right

  
-- I think we're meant to act on a search tree. The exercise isn't very clear on that.
-- I can see how compare is more flexible, but I can't see how it's more efficient (?!)
-- Apparently, the answer is that we do one compare operations wheras we could do 2 before, odd.
occurs :: Ord a => a -> MyTree a -> Bool
occurs x MyLeaf = False
occurs x (MyNode left y right) = case compare x y of 
  EQ -> True
  LT -> occurs x left
  GT -> occurs x right


---

data Tree a = Leaf a
            | Node (Tree a) (Tree a)
            deriving Show

{- 3. Define a function `balanced :: Tree a -> Bool`, for the given `Tree` type

A tree is balanced if the number of leaves in its left and right subtree differs by at most 1.
-}

nodesCount :: Tree a -> Int
nodesCount (Leaf _) = 0
nodesCount (Node left right) = 1 + nodesCount left + nodesCount right

balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node left right) =
  let cnt = (nodesCount left) - (nodesCount right) in
  abs cnt < 2 && balanced left && balanced right

{- 4. Define a function `balance :: [a] -> Tree a` that converts a non-empty
tree to a balanced tree -}

split :: [a] -> ([a], [a])
split =
 (\(xs,ys,_) -> (xs,ys)) . 
   foldl
     (\(xs, ys, toggle) n ->
       if toggle then
         (n:xs, ys, not toggle)
       else
         (xs, n:ys, not toggle)
     )
     ([],[],False)

toTree :: [a] -> Tree a
toTree lst = case lst of
  [] -> error "toTree: empty list"
  x:[] -> Leaf x
  --x:y:[] -> Node (Leaf x) (Leaf y)
  --x:y:rest -> Node (toTree rest) (Node (Leaf x) (Leaf y))
  x:rest -> Node (Leaf x) (toTree rest)


balance :: [a] -> Tree a
balance lst =
  let (xs,ys) = split lst in
  Node (toTree xs) (toTree ys)

---

data Expr
  = Val Int
  | Add Expr Expr
  deriving Show

eval :: Expr -> Int
eval (Val n)   = n
eval (Add a b) = eval a + eval b

{- 5. Define `folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a`
-}

folde :: (Int -> a) -> (a -> a -> a) ->  Expr  -> a
folde            f                g    (Val n)  = f n
folde            f                g  (Add a b)  = g (folde f g a) (folde f g b)

{- 6. Using folde, define:

`eval :: Expr -> Int`
`size :: Expr -> Int`
-}

eval' :: Expr -> Int
eval' = folde id (+)

size :: Expr -> Int
size = folde (const 1) (+)

{-
*Chapter08.Exercises> folde show (\l r -> mconcat ["(", l, " + ", r, ")"]) ((Add (Val 8) (Add (Val 3) (Val 4))))
"(8 + (3 + 4))"
-}

---

{- 7. Complete the following instance declarations -}

data MyMaybe a
   = MyNothing
   | MyJust a
   deriving Show

instance Eq a => Eq (MyMaybe a) where
  MyNothing  == MyNothing  = True
  (MyJust x) == (MyJust y) = x == y

data MyList a
   = MyNil
   | MyCons (a, MyList a)
   deriving Show

instance Eq a => Eq (MyList a) where
  (==) MyNil MyNil = True
  (==) (MyCons (x,xs)) (MyCons (y,ys)) = x == y && xs == ys

---

{- 8. Extend the tautology checker to support disjunction (or): done -}

{- 9. Extend the abstract machine to support multiplication: done -}

