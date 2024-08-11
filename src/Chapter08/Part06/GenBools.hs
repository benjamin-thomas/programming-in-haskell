module Chapter08.Part06.GenBools where

import Chapter07.BinaryStringTransmitter (int2bin)

{-

We want to generate a list of possible combinations of bool values.
We could achieve this by thinking about incrementing a binary number.
For instance, [True, False, True] would represent 101 (5).

>>> int2bin 5
[1,0,1]

>>> take 8 $ (int2bin 5) ++ (repeat 0)
[1,0,1,0,0,0,0,0]
 -}

bools :: Int -> [[Bool]]
bools n = map (reverse . map conv . make n . int2bin) range
 where
  range = [0 .. (2 ^ n - 1)]

  make :: Int -> [Int] -> [Int]
  make n' bs = take n' (bs ++ repeat 0)

  conv 0 = False
  conv 1 = True
  conv _ = error "Not a binary number"

{- Or we can observe that bools3 is just bools2 twice + prepending True and
False for each sub computation, hence a recursive algorithm.

>>> bools 3 == bools' 3
True

>>> bools 4 == bools' 4
True

>>> bools 2
[[False,False],[False,True],[True,False],[True,True]]

>>> bools' 3
[[False,False,False],[False,False,True],[False,True,False],[False,True,True],[True,False,False],[True,False,True],[True,True,False],[True,True,True]]

>>> map (False :) [[True], [False]]
[[False,True],[False,False]]

 -}

bools' :: Int -> [[Bool]]
bools' n
  | n <= 0 = [[]]
  | otherwise =
      let prev = bools' (n - 1)
       in map (False :) prev ++ map (True :) prev
