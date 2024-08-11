module Chapter08.Part06.MyGenBools where

import Control.Monad (replicateM)

bools2 :: [[Bool]]
bools2 = (\a b -> [a, b]) <$> vals <*> vals
 where
  vals = [False, True]

bools3 :: [[Bool]]
bools3 = (\a b c -> [a, b, c]) <$> vals <*> vals <*> vals
 where
  vals = [False, True]

bools4 :: [[Bool]]
bools4 =
  (\a b c d -> [a, b, c, d]) <$> vals <*> vals <*> vals <*> vals
 where
  vals = [False, True]

bools4' :: [[Bool]]
bools4' = replicateM 4 [False, True]

{-

ghci> (:) <$> [False, True] <*> [[]]
[[False],[True]]
ghci> (:) <$> [False, True] <*> [[False],[True]]
[[False,False],[False,True],[True,False],[True,True]]
ghci> (:) <$> [False, True] <*> [[False,False],[False,True],[True,False],[True,True]]
[[False,False,False],[False,False,True],[False,True,False],[False,True,True],[True,False,False],[True,False,True],[True,True,False],[True,True,True]]

 -}
bools4'' :: [[Bool]]
bools4'' = aux 4
 where
  aux :: Int -> [[Bool]]
  aux n
    | n <= 0 = [[]]
    | otherwise = (:) <$> [False, True] <*> aux (n - 1)

{- |

>>> bools4'' == bools4'''
True
-}
bools4''' :: [[Bool]]
bools4''' = aux 4
 where
  aux :: Int -> [[Bool]]
  aux n
    | n <= 0 = [[]]
    | otherwise = [x : xs | x <- [False, True], xs <- aux (n - 1)]

bools3' :: [(Bool, Bool, Bool)]
bools3' =
  (,,) <$> vals <*> vals <*> vals
 where
  vals = [False, True]
