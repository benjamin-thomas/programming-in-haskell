module Chapter08.Part04.Tree3 where

{-

In an expression tree, leaves represent operands (values or variables) and
internal nodes represent operators. The type a could be used for numeric values
(e.g., integers or floats) and the type b could be used for operators (e.g., +, -, *, /).

 -}
data Tree a b
    = Leaf a
    | Node (Tree a b) b (Tree a b)
    deriving (Show)

data Operation
    = Add
    | Sub
    | Mul
    | Div
    deriving (Show)

-- Reminder: `type` defines a type synonym
type Expr = Tree Double Operation

tree :: Expr
tree =
    Node
        (Leaf 1)
        Mul
        ( Node
            (Leaf 3)
            Mul
            ( Node (Leaf 2) Add (Leaf 2)
            )
        )

{- | Maybe flattening could mean transforming into prefix notation...

...but that's actually quite tricky, it'd be better to reshape the tree type.

Instead, I'll implement `eval`.

>>> eval tree
12.0
-}
eval :: Expr -> Double
eval (Leaf x) = x
eval (Node left op right) =
    let
        l = eval left
        r = eval right
     in
        case op of
            Add -> l + r
            Sub -> l - r
            Mul -> l * r
            Div -> l / r
