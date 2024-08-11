module Chapter08.Part04.Trees where

-- We can also define trees

data Tree a
    = Leaf a
    | Node (Tree a) a (Tree a)

tree :: Tree Int
tree =
    Node
        ( Node
            (Leaf 1)
            3
            (Leaf 4)
        )
        5
        ( Node
            (Leaf 6)
            7
            (Leaf 9)
        )

{- |

>>> [occurs 5 tree, occurs 6 tree, occurs 7 tree, occurs 9 tree]
[True,True,True,True]

>>> occurs 99 tree
False
-}
occurs :: (Eq a) => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node left y right) =
    x == y
        || occurs x left
        || occurs x right

{- | If applying `flatten` to a tree returns a sorted list,
then the tree is called a "search tree".

>>> flatten tree
[1,3,4,5,6,7,9]
-}
flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node left x right) =
    mconcat
        [ flatten left
        , [x]
        , flatten right
        ]

{- | A version adapted to search trees. It is more efficient since it traverses
only one path down the tree.

>>> occurs' 6 tree
True

>>> occurs' 4 tree
True

>>> occurs' 99 tree
False
-}
occurs' :: (Eq a, Ord a) => a -> Tree a -> Bool
occurs' x (Leaf y) = x == y
occurs' x (Node left y right) =
    case compare x y of
        EQ -> True
        LT -> occurs' x left
        GT -> occurs' x right

{- As in nature, there are many different forms of trees we can define.
Which one is best depends on the situation.

See Trees1, Trees2, ...
 -}
