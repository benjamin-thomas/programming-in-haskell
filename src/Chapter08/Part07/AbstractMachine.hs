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
  | Mul Expr Expr

{- |
>>> value $ Add (Add (Val 3) (Val 4)) (Val (-1))
6
>>> value $ (Val 3) `Mul` (Val 4)
12
-}
value :: Expr -> Int
value = \case
  (Val n) -> n
  (Add x y) -> value x + value y
  (Mul x y) -> value x * value y

{- With the prior method of evaluation, we did not have precise control over
the evaluation order (left/right).

So we define an "abstract machine" instead, starting with a "control stacks"
-}
type Cont = [Op]

data Op
  = EVAL Expr (Int -> Op)
  | ADD Int
  | MUL Int

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
eval (Add x y) c = eval x (EVAL y ADD : c)
eval (Mul x y) c = eval x (EVAL y MUL : c)

{- |
We define the function that executes a control stack in the context of an
integer argument

>>> exec (ADD 1 : ADD 2 : ADD 3 : []) 0
6

>>> exec (MUL 3 : MUL 4 : []) 1
12

>>> exec (EVAL (Add (Val 3) (Val 4)) ADD : []) 0
7

>>> exec (ADD 1 : EVAL (Add (Val 3) (Val 4)) ADD : []) 0
8
-}
exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL y op : c) n = eval y (op n : c)
exec (ADD n : c) m = exec c (n + m)
exec (MUL n : c) m = exec c (n * m)

{- |
>>> value' (Add (Add (Val 2) (Val 3)) (Val 4))
9

>>> eval (Add (Add (Val 2) (Val 3)) (Val 4)) []
9

>>> eval (Add (Val 2) (Val 3)) (EVAL (Val 4) ADD : [])
9

>>> eval (Val 2) (EVAL (Val 3) ADD : EVAL (Val 4) ADD : [])
9

>>> exec (EVAL (Val 3) ADD : EVAL (Val 4) ADD : []) 2
9

>>> eval (Val 3) (ADD 2 : EVAL (Val 4) ADD : [])
9

>>> exec (ADD 2 : EVAL (Val 4) ADD : []) 3
9

    exec (EVAL (Val 4) ADD : []) (2+3)
>>> exec (EVAL (Val 4) ADD : []) 5
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


{-

So... to sum up...

This:

ghci> value (Add (Val 2) (Val 3))
5

Is the same as:

ghci> eval (Add (Val 2) (Val 3)) []
5

Except in the latter, we control the order of operations via a "control stack"

eval (Add (Val 2) (Val 3)) []
=
eval (Val 2) (EVAL (Val 3) ADD : [])
=
exec (EVAL (Val 3) ADD : []) 2
=
eval (Val 3) (ADD 2 : [])
=
exec (ADD 2 : []) 3
=
exec [] (2+3)
=
exec [] 5
5


-}


{-

Another manual trace
====================

*MyLib Chapter08.Part07.AbstractMachine> eval (Add (Val 2) (Val 3)) (EVAL (Val 4) ADD:[])
9
*MyLib Chapter08.Part07.AbstractMachine> eval (Val 2) (EVAL (Val 3) ADD : EVAL (Val 4) ADD:[])
9
*MyLib Chapter08.Part07.AbstractMachine> exec (EVAL (Val 3) ADD : EVAL (Val 4) ADD:[]) 2
9
*MyLib Chapter08.Part07.AbstractMachine> eval (Val 3) (ADD 2 : EVAL (Val 4) ADD:[])
9
*MyLib Chapter08.Part07.AbstractMachine> exec (ADD 2 : EVAL (Val 4) ADD:[]) 3
9
*MyLib Chapter08.Part07.AbstractMachine> exec (EVAL (Val 4) ADD:[]) (2+3)
9
*MyLib Chapter08.Part07.AbstractMachine> exec (EVAL (Val 4) ADD:[]) 5
9
*MyLib Chapter08.Part07.AbstractMachine> eval (Val 4) (ADD 5 : [])
9
*MyLib Chapter08.Part07.AbstractMachine> exec (ADD 5 : []) 4
9
*MyLib Chapter08.Part07.AbstractMachine> exec [] (5+4)
9
*MyLib Chapter08.Part07.AbstractMachine> exec [] 9
9
-}
