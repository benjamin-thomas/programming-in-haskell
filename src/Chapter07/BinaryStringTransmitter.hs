module Chapter07.BinaryStringTransmitter where

import Data.Char

-- echo ./src/Chapter07/BinaryStringTransmitter.hs | entr -c doctest /_

{- |

Overview
========

We will encode ints to a binary representation.

While we could encode ten as [1,0,1,0]:
>>> 0b1010
10

We will actually encode it in reverse, like so: [0,1,0,1].

Also, we will use iterate, which builds an infinite list by repeatedly applying `f`:

iterate f x = [x, f x, f (f x), f (f (f x)), ...]

>>> take 10 $ iterate (*2) 1
[1,2,4,8,16,32,64,128,256,512]
-}
type Bit = Int -- type alias

{- |
>>> bin2int [0,1,0,1]
10

>>> bin2int [1,0,1,1]
13
-}
bin2int :: [Bit] -> Int
bin2int bits = sum [w * b | (w, b) <- zip weights bits]
 where
  weights = iterate (* 2) 1

{-

There is, however, a simpler way to define `bin2int` which can be revealed with
the aid of some algebra.

Consider applying `bin2int` to [a,b,c,d]:

= (1*a) + (2*b) + (4*c) + (8*d)                   -- orig
= a + (2*b) + (4*c) + (8*d)                       -- simplified 1*
= a + 2 * (b + (2*c) + (4*d))                     -- factored out 2*
= a + 2 * (b + 2 * (c + (2*d)))                   -- factored out 2*
= a + 2 * (b + 2 * (c + 2 * (d + 2 * 0)))         -- complicated d

The final result shows that converting a list of bits [a,b,c,d] into an integer
amounts to replacing each cons by the function that adds its first argument to
twice its second argument, and replacing the empty list by zero.

We can thus represent this operation with `foldr`

>>> bin2int' [0,1,0,1]
10

>>> bin2int' [1,0,1,1]
13
 -}

bin2int' :: [Bit] -> Int
bin2int' = foldr (\x y -> x + 2 * y) 0

{- |

For the reverse operation, we will repeatedly divide the integer by 2 and take the
reminder until the integer becomes 0.

13 / 2 = 6 => remainder 1
 6 / 2 = 3 => remainder 0
 3 / 2 = 1 => remainder 1
 1 / 2 = 0 => remainder 1

=> result is [1,0,1,1]

>>> int2bin 13
[1,0,1,1]
-}
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

{- |

We will ensure that all our binary numbers have the same length weth `repeat`,
which generates an infinite list

>>> take 8 (repeat 0)
[0,0,0,0,0,0,0,0]

>>> take 8 $ [1..3] ++ repeat 0
[1,2,3,0,0,0,0,0]

>>> make8 [1,0,1,1]
[1,0,1,1,0,0,0,0]
-}
make8 :: [Bit] -> [Bit]
make8 bits = take 8 $ bits ++ repeat 0

{- |

Add parity bit

>>> parity []
[0]

>>> parity [0]
[0,0]

>>> parity [1]
[1,1]

>>> parity [1,0,1,1,0,0,0,0]
[1,1,0,1,1,0,0,0,0]

>>> parity [1,0,0,1,0,0,0,0]
[0,1,0,0,1,0,0,0,0]
-}
parity :: [Bit] -> [Bit]
parity bits
  | odd $ length $ filter (== 1) bits = 1 : bits
  | otherwise = 0 : bits

{- |

>>> make8 $ int2bin $ ord 'a'
[1,0,0,0,0,1,1,0]

>>> make8 $ int2bin $ ord 'b'
[0,1,0,0,0,1,1,0]

>>> make8 $ int2bin $ ord 'c'
[1,1,0,0,0,1,1,0]

>>> (make8 . int2bin . ord) 'a'
[1,0,0,0,0,1,1,0]

>>> map (make8 . int2bin . ord) "abc"
[[1,0,0,0,0,1,1,0],[0,1,0,0,0,1,1,0],[1,1,0,0,0,1,1,0]]

>>> concat $ map (make8 . int2bin . ord) "abc"
[1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]

>>> (concat . map (make8 . int2bin . ord)) "abc"
[1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]

>>> concatMap (make8 . int2bin . ord) "abc"
[1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]

>>> encode "abc"
[1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]

--- Add parity bit

>>> (parity . make8 . int2bin . ord) 'a'
[1,1,0,0,0,0,1,1,0]

>>> concatMap (parity . make8 . int2bin . ord) "abc"
[1,1,0,0,0,0,1,1,0,1,0,1,0,0,0,1,1,0,0,1,1,0,0,0,1,1,0]

[^ 1,0,0,0,0,1,1,0,^ 0,1,0,0,0,1,1,0,^ 1,1,0,0,0,1,1,0] <-- from orig

>>> encode' "abc"
[1,1,0,0,0,0,1,1,0,1,0,1,0,0,0,1,1,0,0,1,1,0,0,0,1,1,0]
-}
encode :: String -> [Bit]
encode = concatMap (make8 . int2bin . ord)

encode' :: String -> [Bit]
encode' = concatMap (parity . make8 . int2bin . ord)

{-

>>> chop8 []
[]

>>> chop8 [1..4]
[[1,2,3,4]]

>>> chop8 [1..10]
[[1,2,3,4,5,6,7,8],[9,10]]

 -}

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

{- |

chop the next 8 bits, while ensuring bit parity is maintained. Crashes otherwise
-}
chop8' :: [Int] -> [[Bit]]
chop8' bits =
  case take 9 bits of
    [] -> []
    (x : xs) ->
      let
        x' = if odd $ length $ filter (== 1) xs then 1 else 0
       in
        if x /= x'
          then error "Invalid parity"
          else xs : chop8' (drop 9 bits)

{- |


>>> map bin2int' $ chop8 [1..16]
[1793,3833]

>>> (map bin2int' . chop8) [1..16]
[1793,3833]

>>> decode [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]
"abc"
-}
decode :: [Bit] -> String
decode = map (chr . bin2int') . chop8

{- | Decode with parity

>>> map bin2int' $ chop8' [1,1,0,0,0,0,1,1,0]
[97]

>>> map (chr .bin2int') $ chop8' [1,1,0,0,0,0,1,1,0]
"a"

>>> decode' [1,1,0,0,0,0,1,1,0,1,0,1,0,0,0,1,1,0,0,1,1,0,0,0,1,1,0]
"abc"

>>> decode' [0,1,0,0,0,0,1,1,0,1,0,1,0,0,0,1,1,0,0,1,1,0,0,0,1,1,0]
"*** Exception: Invalid parity
...
-}
decode' :: [Bit] -> String
decode' = map (chr . bin2int') . chop8'

{- |

>>> transmit "Hello, World!"
"Hello, World!"

Simulated channel

>>> encode "hello"
[0,0,0,1,0,1,1,0,1,0,1,0,0,1,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0,1,1,0,1,1,1,1,0,1,1,0]

>>> decode $ encode "hello"
"hello"

>>> decode $ id $ encode "hello"
"hello"

---

With parity check

>>> decode' $ id $ encode' "hello"
"hello"

>>> decode' $ channel $ encode' "hello"
"hello"

>>> decode' $ faultyChannel $ encode' "hello"
"*** Exception: Invalid parity
...
-}

-- We simulate a perfect communication channel
channel :: [Bit] -> [Bit]
channel = id

-- We simulate a faulty communication channel
faultyChannel :: [Bit] -> [Bit]
faultyChannel = tail

transmit :: String -> String
transmit = decode . channel . encode
