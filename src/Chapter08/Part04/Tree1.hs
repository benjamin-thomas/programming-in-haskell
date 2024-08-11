module Chapter08.Part04.Tree1 where

{-
I can only express pairs of values (only even number of values).
It seems similar to a list.
I must travel every nodes to reach any value.
 -}
data Tree a
    = Leaf a
    | Node (Tree a) (Tree a)
    deriving (Show)

tree :: Tree Int
tree =
    Node
        ( Node
            ( Node
                (Leaf 1)
                (Leaf 2)
            )
            ( Node
                (Leaf 3)
                (Leaf 4)
            )
        )
        ( Node
            ( Node
                (Leaf 5)
                (Leaf 6)
            )
            ( Node
                (Leaf 7)
                (Leaf 8)
            )
        )

{- | If applying `flatten` to a tree returns a sorted list,
then the tree is called a "search tree".

>>> flatten tree
[1,2,3,4,5,6,7,8]
-}
flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node left right) =
    mconcat
        [ flatten left
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
occurs x (Node left right) =
    occurs x left
        || occurs x right
