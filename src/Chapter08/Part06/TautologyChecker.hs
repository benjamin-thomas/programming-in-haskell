{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use all" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
module Chapter08.Part06.TautologyChecker where

import Chapter08 (Assoc, find)

{-
  Here are the common logic symbols:

  - ∧ = AND (conjunction)
  - ∨ = OR (disjunction)
  - ¬ = NOT (negation)
  - → = IMPLIES (implication)
-}

data Prop
    = Const Bool
    | Var   Char
    | Not   Prop
    | And   Prop Prop
    | Or    Prop Prop
    | Imply Prop Prop
    deriving Show

-- A ∧ ¬A
p1 :: Prop
p1 =
    And
        (Var 'A')
        (Not (Var 'A'))

p1' :: Prop
p1' =
    Var 'A' `And` Not (Var 'A')

p2 :: Prop
p2 =
    Imply
        ( And
            (Var 'A')
            (Var 'B')
        )
        (Var 'A')

p2' :: Prop
p2' =
    (Var 'A' `And` Var 'B') `Imply` Var 'A'

p3 :: Prop
p3 =
    Imply
        (Var 'A')
        (And (Var 'A') (Var 'B'))

p3' :: Prop
p3' =
    Var 'A' `Imply` (Var 'A' `And` Var 'B')

p4 :: Prop
p4 =
    Imply
        ( And
            (Var 'A')
            (Imply (Var 'A') (Var 'B'))
        )
        (Var 'B')

p4' :: Prop
p4' =
    ( Var 'A'
        `And` (Var 'A' `Imply` Var 'B')
    )
        `Imply` Var 'B'

-- Classic tautology: Law of Excluded Middle
-- "A is either true or false" - always true!
p5 :: Prop
p5 = Var 'A' `Or` Not (Var 'A')

-- De Morgan's Law (tautology)
-- "Not (A and B) is equivalent to (Not A or Not B)"
p6 :: Prop
p6 = Not (Var 'A' `And` Var 'B') `Imply` (Not (Var 'A') `Or` Not (Var 'B'))


{- In order to evaluate a proposition to a logical value, we need to know the
value of each of its variables. For this reason, we declare a lookup table that
associates variable names to logical values.
 -}
type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find x s -- may crash
eval s (Not p) = not (eval s p)
eval s (And a b) = eval s a && eval s b
eval s (Or a b) = eval s a || eval s b
eval s (Imply p q) = eval s p <= eval s q

{- To decide if a proposition is a tautology, we will consider all substitutions
for the variables that it contains.
-}

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And a b) = vars a <> vars b
vars (Or a b) = vars a <> vars b
vars (Imply p q) = vars p <> vars q

-- See Chapter08.Part06.MyGenBools and Chapter08.Part06.GenBools
bools :: Int -> [[Bool]]
bools n
    | n <= 0 = [[]]
    | otherwise =
        let prev = bools (n - 1)
         in map (False :) prev ++ map (True :) prev

bools' :: Int -> [[Bool]]
bools' = aux
  where
    aux :: Int -> [[Bool]]
    aux n
        | n <= 0 = [[]]
        | otherwise = (:) <$> [False, True] <*> aux (n - 1)

{- |

>>> uniq ["Red", "Blue", "Green", "Blue", "Blue", "Red"]
["Red","Blue","Green"]
-}
uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq (x : xs) = x : filter (/= x) (uniq xs)

{-

We can now generate all possible substitutions for a proposition by extracting
its variables, whilst removing duplicate values

 -}
substitutions :: Prop -> [Subst]
substitutions p = map (zip vs) (bools' (length vs))
  where
    vs = uniq (vars p)

{- Then we check if the proposition is a tautology by checking if all possible
substitutions are True.

>>> map isTaut [p1,p2,p3,p4]
[False,True,False,True]

>>> map isTaut2 [p1,p2,p3,p4]
[False,True,False,True]

>>> map isTaut3 [p1,p2,p3,p4]
[False,True,False,True]

-}

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substitutions p]

isTaut2 :: Prop -> Bool
isTaut2 p = and $ (\s -> eval s p) <$> substitutions p

isTaut3 :: Prop -> Bool
isTaut3 p = all (`eval` p) (substitutions p)
