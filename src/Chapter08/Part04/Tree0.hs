module Chapter08.Part04.Tree0 where

-- Recap from the main lesson (Trees.hs)

data Tree a
    = Leaf a
    | Node (Tree a) a (Tree a)

tree :: Tree Int
tree =
    Node
        ( Node
            (Leaf 1)
            2
            (Leaf 3)
        )
        4
        ( Node
            (Leaf 5)
            6
            (Leaf 7)
        )

{- | If applying `flatten` to a tree returns a sorted list,
then the tree is called a "search tree".

>>> flatten tree
[1,2,3,4,5,6,7]
-}
flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
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
occurs x (Leaf y) = x == y
occurs x (Node left y right) =
    case compare x y of
        EQ -> True
        LT -> occurs x left
        GT -> occurs x right
