{-# LANGUAGE LambdaCase #-}

module Chapter08.Part07.AbstractMachine where

{-

\$ cabal repl
> :m Chapter08.Part07.AbstractMachine

---

doctest ./src/Chapter08/Part07/AbstractMachine.hs

 -}

data Expr
  = Val Int
  | Add Expr Expr

{- |
>>> value $ Add (Add (Val 3) (Val 4)) (Val (-1))
6
-}
value :: Expr -> Int
value = \case
  (Val n) -> n
  (Add x y) -> value x + value y

{- With the prior method of evaluation, we did not have precise control over
the evaluation order (left/right).

So we define an "abstract machine" instead, starting with a "control stacks"
-}
type Cont = [Op]

data Op
  = EVAL Expr
  | ADD Int

{-
We now define a function that evaluates an expression in the context of a control stacks.

If the expression is an integer, then it is already fully evaluated, and we begin
executing the control stacks.

If the expression is an addition, we evaluate the first argument `x`, placing the
operation `EVAL y` on top of the control stack, to indicate that the second
argument `y` should be evaluated, once the evaluation of the first argument is
completed.
-}
eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVAL y : c)

{- |
We define the function that executes a control stack in the context of an
integer argument

>>> exec (ADD 1 : ADD 2 : ADD 3 : []) 0
6

>>> exec (EVAL (Add (Val 3) (Val 4)) : []) 0
7

>>> exec (ADD 1 : EVAL (Add (Val 3) (Val 4)) : []) 0
8
-}
exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD n : c) m = exec c (n + m)

{- |
>>> value' (Add (Add (Val 2) (Val 3)) (Val 4))
9

>>> eval (Add (Add (Val 2) (Val 3)) (Val 4)) []
9

>>> eval (Add (Val 2) (Val 3)) (EVAL (Val 4) : [])
9

>>> eval (Val 2) (EVAL (Val 3) : EVAL (Val 4) : [])
9

>>> exec (EVAL (Val 3) : EVAL (Val 4) : []) 2
9

>>> eval (Val 3) (ADD 2 : EVAL (Val 4) : [])
9

>>> exec (ADD 2 : EVAL (Val 4) : []) 3
9

    exec (EVAL (Val 4) : []) (2+3)
>>> exec (EVAL (Val 4) : []) 5
9

>>> eval (Val 4) (ADD 5 : [])
9

>>> exec (ADD 5 : []) 4
9

    exec [] (5+4)
>>> exec [] 9
9
-}
value' :: Expr -> Int
value' e = eval e []
