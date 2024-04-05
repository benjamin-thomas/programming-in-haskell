{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use uncurry" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use list literal" #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Chapter07.Exercises where

import Debug.Trace (trace)
import Text.Printf (printf)
import Prelude hiding (all, any, curry, dropWhile, filter, map, takeWhile, uncurry)

-- echo ./src/Chapter07/Exercises.hs | entr -c doctest /_

{- |

1) Show how the list comprehension `[f x | x <- xs, p x]` can be re-expressed
   using the higher-order functions `map` and `filter`.


   f x <-- that's mapping from `x <- xs`
   p x <-- that's the filtering condition


>>> [id x | x <- [1..10], even x]
[2,4,6,8,10]

>>> [(*2) x | x <- [1..10], even x]
[4,8,12,16,20]

>>> map (*2) $ filter even [1..10]
[4,8,12,16,20]
-}

-- 2) Implement all, any, takeWhile, dropWhile.

{- |

>>> all even []
True

>>> all odd []
True

>>> all (<10) [1,2,3,4,5,6]
True

>>> all even [1,2,3,4,5,6]
False
-}
all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all f (x : xs) = f x && all f xs

{- |

>>> any even []
False

>>> any odd []
False

>>> any (>3) [1,2,3,4,5,6]
True

>>> any (<0) [1,2,3,4,5,6]
False
-}
any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any f (x : xs) = f x || any f xs

{- |

>>> takeWhile id []
[]

>>> takeWhile (< 0) [1,2,3]
[]

>>> takeWhile (< 3) [1,2,3,4,1,2,3,4]
[1,2]

>>> takeWhile (< 9) [1,2,3]
[1,2,3]
-}
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile f (x : xs) =
  if f x
    then x : takeWhile f xs
    else []

{- |

>>> dropWhile id []
[]

>>> dropWhile (< 3) [1,2,3,4,5,1,2,3]
[3,4,5,1,2,3]

>>> dropWhile (< 9) [1,2,3]
[]

>>> dropWhile (< 0) [1,2,3]
[1,2,3]
-}
dropWhile _ [] = []
dropWhile f (x : xs) =
  if f x
    then dropWhile f xs
    else x : dropWhile (const False) xs

{- |

3) Redefine `map` and `filter` using `foldr`

>>> foldr (\x acc -> x : acc) [] [1,2,3]
[1,2,3]

>>> foldr (\x acc -> (*2) x : acc) [] [1,2,3]
[2,4,6]


>>> map (*2) []
[]

>>> map (*2) [1,2,3]
[2,4,6]


>>> filter even [1..10]
[2,4,6,8,10]

>>> filter odd [1..10]
[1,3,5,7,9]
-}
map :: (a -> b) -> [a] -> [b]
map f = foldr (\x acc -> f x : acc) []

filter :: (a -> Bool) -> [a] -> [a]
filter f =
  foldr
    (\x acc -> if f x then x : acc else acc)
    []

{- |

4) Using `foldl`, define a function `dec2int :: [Int] -> Int` that converts a
   decimal number into an integer. For example:

>>> dec2int [2,3,4,5]
2345

>>> dec2int []
0

ghci> 5*1 + 4*10 + 3*100 + 2*1000
2345

ghci> foldl (\(n, acc) x -> (n - 1, acc + x * (10 ^ (n -1)))) (4, 0) [2, 3, 4, 5]
(0,2345)
-}
dec2int :: (Foldable t, Num a) => t a -> a
dec2int xs =
  snd
    $ foldl
      ( \(n, acc) x ->
          ( n - 1
          , acc + x * (10 ^ n)
          )
      )
      (length xs - 1, 0)
      xs

{- |

Second attempt
==============

What I need to do:
For 2345
ghci> 5*1 + 4*10 + 3*100 + 2*1000
2345

>>> take 4 $ iterate (*10) 1
[1,10,100,1000]

Okay, but wrong way around
>>> zip [2,3,4,5] $ iterate (*10) 1
[(2,1),(3,10),(4,100),(5,1000)]


Problem: I would need to traverse the list to compute "1000". Not great.
>>> zip [2,3,4,5] $ iterate (\x -> x `div` 10) 1000
[(2,1000),(3,100),(4,10),(5,1)]


I guess that's as good as I can do?
>>> zip (iterate (*10) 1) $ reverse [2,3,4,5]
[(1,5),(10,4),(100,3),(1000,2)]

>>> (zip (iterate (*10) 1) . reverse) [2,3,4,5]
[(1,5),(10,4),(100,3),(1000,2)]


>>> foldl (\acc (n,m) -> acc + n*m ) 0 (zip (iterate (*10) 1) $ reverse [2,3,4,5])
2345

>>> dec2int' [2,3,4,5]
2345

>>> dec2int' []
0
-}
dec2int' :: (Num a) => [a] -> a
dec2int' xs =
  foldl
    (\acc (n, m) -> acc + n * m)
    0
    (zip (iterate (* 10) 1) $ reverse xs)

{-
The book's solution 🤦‍♂️️

>>> dec2int'' []
0

>>> dec2int'' [2, 3, 4, 5]
2345

foldl (\x y -> 10 * x + y) 0 [2,3,4,5]

 -}

dec2int'' :: (Num a) => [a] -> a
dec2int'' = foldl (\x y -> 10 * x + y) 0

{- |


Break down of the books solution


ghci> foldl (\acc n -> 10 * acc + n) 0 $ [2,3,4,5]
2345

Ruby equivalent:
[2,3,4,5].reduce { |acc,n| 10 * acc + n }

> (10*0+2)

  (10 × 0) + 2 = 2

> (10*2+3)

  (10 × 2) + 3 = 23

> (10*23+4)

  (10 × 23) + 4 = 234

> (10*234+5)

  (10 × 234) + 5 = 2345

ghci> 10*0+(head [2,3,4,5])
2
ghci> 10*2+(head [3,4,5])
23
ghci> 10*23+(head [4,5])
234
ghci> 10*234+(head [5])
2345
-}

{- |

5) Write `curry` and `uncurry`

>>> (\(a, b) -> a + b) (1, 2)
3

>>> (curry (\(a, b) -> a + b)) 1 2
3

>>> uncurry (curry (\(a, b) -> a + b)) (1, 2)
3
-}
curry :: ((a, b) -> c) -> a -> b -> c
curry = \f a b -> f (a, b)

uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry = \f (a, b) -> f a b

add :: (Int, Int) -> Int
add (a, b) = a + b

add' :: Int -> Int -> Int
add' = curry add

mul :: Int -> Int -> Int
mul a b = a * b

mul' :: (Int, Int) -> Int
mul' = uncurry mul

{- |

6) Given the function `unfold`, rewrite `chop8` `map f` and `iterate f` using `unfold`.

`unfold` stops producing the list if the predicate `p` is true, otherwise applies `h`
to produce the next head and applies `t` to produce the next tail that will be
processed in the same manner.

See `int2bin` as an example.

Orig implementations
--------------------
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

>>> int2bin 13
[1,0,1,1]

>>> unfold (null) (take 2) (drop 2) [1..5]
[[1,2],[3,4],[5]]

>>> chop8 [1..17]
[[1,2,3,4,5,6,7,8],[9,10,11,12,13,14,15,16],[17]]

>>> unfold null ((*2) . head) tail $ [1,2,3]
[2,4,6]

>>> map' (*2) [1,2,3]
[2,4,6]

>>> take 3 $ iterate (+1) 10
[10,11,12]

>>> take 3 $ unfold (const False) ((+1) . head) tail $ [0..]
[1,2,3]

>>> take 3 $ iterate (*2) 10
[10,20,40]

>>> take 3 $ unfold (const False) (head) (\xs -> head xs * 2 : tail xs) $ [10..]
[10,20,40]

>>> take 3 $ unfold (const False) (head) (\xs -> ((:) . (*2)) ((head xs)) (tail xs)) $ [10..]
[10,20,40]

>>> take 8 $ iterate (*2) 1
[1,2,4,8,16,32,64,128]

>>> take 8 $ iterate' (*2) 1
[1,2,4,8,16,32,64,128]
-}
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

int2bin :: Integer -> [Integer]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

chop8 :: [a] -> [[a]]
chop8 = unfold null (take 8) (drop 8)

map' :: (a -> b) -> [a] -> [b]
map' f = unfold null (f . head) tail

iterate' :: (Enum a) => (a -> a) -> a -> [a]
iterate' f start = unfold (const False) head (\xs -> f (head xs) : []) [start ..]

{-
7) Extend the binary string transmitter example with error detection
8) Test it with a fake faulty communication channel
-}

{- |
9) Define `altMap`

>>> altMap (+10) (+100) [0,1,2,3,4]
[10,101,12,103,14]
-}
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g [] = []
altMap f g xs = head (map f xs) : altMap g f (tail xs)

{- |

Using `altMap`, implement `luhn :: [Int] -> Bool` from chapter 4

>>> luhn [7, 9, 9, 2, 7, 3, 9, 8, 7, 1, 3]
True

>>> luhn [8, 9, 9, 2, 7, 3, 9, 8, 7, 1, 3]
False
-}
luhn :: [Int] -> Bool
luhn ns =
  sum run `mod` 10 == 0
 where
  cap n = if n > 9 then n - 9 else n
  run = altMap id (cap . (* 2)) ns