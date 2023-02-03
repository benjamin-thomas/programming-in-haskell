module Chapter02 where

{- | Select first element of a list
>>> head [1,2,3]
1
-}

{- | Select rest
>>> tail [1,2,3]
[2,3]
-}

{- | Select the nth element of a list
>>> [1,2,3] !! 0
1
>>> [1,2,3] !! 1
2
-}

{- | Select the first x elements of a list
>>> take 3 [1,2,3,4,5]
[1,2,3]
-}

{- | Select the last x elements of a list
>>> drop 3 [1,2,3,4,5]
[4,5]
-}

{- | Other self-explaining functions
>>> length [1,2,3]
3

>>> sum [1,2,3]
6

>>> product [2,3,4]
24

>>> [1,2,3] ++ [4,5]
[1,2,3,4,5]

>>> reverse [1,2,3,4,5]
[5,4,3,2,1]
-}
