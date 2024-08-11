module Chapter08.Part04.Tree2 where

data Tree a
    = Leaf
    | Node (Tree a) a (Tree a)

tree :: Tree Int
tree =
    Node
        ( Node
            (Node Leaf 1 Leaf)
            2
            (Node Leaf 3 Leaf)
        )
        4
        ( Node
            (Node Leaf 5 Leaf)
            6
            (Node Leaf 7 Leaf)
        )

{- | If applying `flatten` to a tree returns a sorted list,
then the tree is called a "search tree".

>>> flatten tree
[1,2,3,4,5,6,7]
-}
flatten :: Tree a -> [a]
flatten Leaf = []
flatten (Node left x right) =
    mconcat
        [ flatten left
        , [x]
        , flatten right
        ]

{- |
>>> occurs 6 tree
True

>>> occurs 4 tree
True

>>> occurs 99 tree
False
-}
occurs :: (Eq a, Ord a) => a -> Tree a -> Bool
occurs _ Leaf = False
occurs x (Node left y right) =
    case compare x y of
        EQ -> True
        LT -> occurs x left
        GT -> occurs x right
