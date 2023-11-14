{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use isAsciiLower" #-}

module Chapter05.Caesar where

import Data.Char (chr, isLower, isUpper, ord)
import Data.List (sort)

{- |

>>> let2int 'a'
0

>>> let2int 'b'
1

>>> let2int 'c'
2
-}
let2int :: Char -> Int
let2int c = ord c - ord 'a'

{- |

>>> let2int' 'A'
0

>>> let2int' 'B'
1

>>> let2int' 'C'
2
-}
let2int' :: Char -> Int
let2int' c = ord c - ord 'A'

{- |

>>> int2let 0
'a'

>>> int2let 1
'b'

>>> int2let 2
'c'
-}
int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

{- |

>>> int2let' 0
'A'

>>> int2let' 1
'B'

>>> int2let' 2
'C'
-}
int2let' :: Int -> Char
int2let' n = chr (ord 'A' + n)

{- |
>>> shift 0 'a'
'a'

>>> shift 0 'A'
'A'

>>> shift 1 'a'
'b'

>>> shift 25 'a'
'z'

>>> shift 26 'a'
'a'

>>> shift 13 '!'
'!'
-}
shift :: Int -> Char -> Char
shift n c
  | isLower c = int2let $ (let2int c + n) `mod` 26
  | isUpper c = int2let' $ (let2int' c + n) `mod` 26
  | otherwise = c

{- |

>>> encode 0 "abc"
"abc"

>>> encode 1 "abc"
"bcd"

>>> encode 2 "abc"
"cde"

>>> encode 3 "Hello, world!"
"Khoor, zruog!"

>>> encode (-3) "Khoor, zruog!"
"Hello, world!"

>>> encode 0 "AaBbCc!"
"AaBbCc!"

>>> encode 1 "AaBbCc!"
"BbCcDd!"

>>> encode 2 "AaBbCc!"
"CcDdEe!"

>>> encode 25 "AaBbCc!"
"ZzAaBb!"
-}
encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

{- | Handles lower/uppercase (exercise 10)

>>> encode (-3) "Khoor, zruog!"
"Hello, world!"
-}
encode' :: Int -> String -> String
encode' n xs = [shift n x | x <- xs]

-- FREQUENCY TABLES

-- 26 most common letters in English, by weights
{- FOURMOLU_DISABLE -}
table :: [Float]
table =
    [ 8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4
    , 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1
    ]
{- FOURMOLU_ENABLE -}

{- | Calculates the percentage of one integer with respect to another

>>> percent 2 4
50.0

>>> percent 3 4
75.0

>>> percent 1 3
33.333336
-}
percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

-- From Chapter05
lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

-- From Chapter05
count :: Char -> String -> Int
count x xs = length [x | x' <- xs, x == x']

{- | Using `percent` within a list comprehension, together with the function
`lowers` and `count` from the previous section, we can now define a function
that returns a frequency table for any given string

>>> freqs "hello"
[0.0,0.0,0.0,0.0,20.0,0.0,0.0,20.0,0.0,0.0,0.0,40.0,0.0,0.0,20.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
-}
freqs :: String -> [Float]
freqs xs =
  [percent (count x xs) n | x <- ['a' .. 'z']]
 where
  n = lowers xs

--- CRACKING THE CIPHER

{- | A standard method for comparing a list of observed frequencies `os` with a
list of expected frequencies `es` is the "chi-square statistic"

http://asciimath.org/
    sum_(i=0)^(n-1) (os_i - es_i)^2/(es_i)
-}
chiSquare :: [Float] -> [Float] -> Float
chiSquare os es = sum [((o - e) ^ 2) / e | (o, e) <- zip os es]

{- | We define a function that rotates the elements of a list `n` places to the left.
Assuming `n` is between zero and the length of the list.

>>> rotate 1 [1,2,3,4]
[2,3,4,1]

>>> rotate 2 [1,2,3,4]
[3,4,1,2]

>>> rotate 3 [1,2,3,4]
[4,1,2,3]

>>> rotate 4 [1,2,3,4]
[1,2,3,4]

>>> rotate 5 [1,2,3,4]
[1,2,3,4]
-}
rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

-- From Chapter05
positions :: (Eq a) => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0 ..], x == x']

{- | Now, suppose we are given an encoded string but not the shift factor which we want to find.
We can produce a frequency table of the encoded string, and calculate the chi-square statistic
for each possible rotation and use the smallest result as a shift factor.

>>> crack "kdvnhoo lv ixq"
"haskell is fun"

>>> crack "vscd mywzboroxcsyxc kbo ecopev"
"list comprehensions are useful"

It fails on some inputs:
>>> crack (encode 3 "haskell")
"piasmtt"

>> crack "Hhoor, zruog!"
"Htaad, ldgas!"
-}
crack :: String -> String
crack xs = encode (-factor) xs
 where
  factor = head (positions (minimum chiTab) chiTab)
  chiTab = [chiSquare (rotate n table') table | n <- [0 .. 25]]
  table' = freqs xs
