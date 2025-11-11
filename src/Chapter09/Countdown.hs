-- cabal repl --build-depends pretty-simple
-- :m Text.Pretty.Simple Chapter09.Countdown
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use concatMap" #-}
module Chapter09.Countdown where

import Data.Maybe (listToMaybe)

data Op
  = Add
  | Sub
  | Mul
  | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

-- Would applying the operation return a positive integer?
-- Inputs are always positive
valid_ :: Op -> Int -> Int -> Bool
valid_ Add _ _ = True
valid_ Sub x y = x > y
valid_ Mul _ _ = True
valid_ Div x y = x `mod` y == 0 -- output could be fractional otherwise


{-| Same as `valid_`, but exploits the algebraic laws

x + y = y + x
x * y = y * x
x * 1 = x
1 * y = y
x / 1 = x

-}
valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 1 && x `mod` y == 0


apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr
  = Val Int
  | App Op Expr Expr

instance Show Expr where
  show (Val n) = show n
  show (App op e1 e2) = "(" <> show e1 <> show op <> show e2 <> ")"

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ e1 e2) = values e1 <> values e2

-- By convention, a singleton list indicates success, the empty list indicates failure
eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App op e1 e2) =
  [ apply op a b
  | a <- eval e1
  , b <- eval e2
  , valid op a b
  ]

eval' :: Expr -> Maybe Int
eval' = listToMaybe . eval

--- Combinatorial functions

{- | Return all subsequences of a list.

>>> import Data.List (sortOn)
>>> sortOn (\xs -> (length xs, xs)) (subs [1,2,3])
[[],[1],[2],[3],[1,2],[1,3],[2,3],[1,2,3]]

>>> import Control.Arrow ((&&&))
>>> sortOn (length &&& id) (subs [1,2,3])
[[],[1],[2],[3],[1,2],[1,3],[2,3],[1,2,3]]

subs [1,2,3]
=
(subs [2,3]) <> map (1:) (subs [2,3])
=
(subs [3] <> map (2:) (subs [3])) <> map (1:) (subs [3] <> map (2:) (subs [3]))
=
((subs [] <> map (3:) (subs [])) <> map (2:) ((subs [] <> map (3:) (subs [])))) <> map (1:) ((subs [] <> map (3:) (subs [])) <> map (2:) ((subs [] <> map (3:) (subs []))))
=
(([[]] <> map (3:) ([[]])) <> map (2:) (([[]] <> map (3:) ([[]])))) <> map (1:) (([[]] <> map (3:) ([[]])) <> map (2:) (([[]] <> map (3:) ([[]]))))
=
([[],[3]] <> map (2 :) ([[],[3]])) <> map (1 :) ([[],[3]] <> map (2 :) ([[],[3]]))
=
([[], [3]] <> [[2], [2, 3]]) <> map (1 :) ([[], [3]] <> [[2], [2, 3]])
=
([[], [3], [2], [2, 3]]) <> map (1 :) ([[], [3], [2], [2, 3]])
=
([[], [3], [2], [2, 3]]) <> [[1],[1,3],[1,2],[1,2,3]]
=
[[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]
-}
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x : xs) =
  more <> map (x :) more
 where
  more = subs xs

subs00 :: [[Int]]
subs00 = subs [1, 2, 3]

subs01 :: [[Int]]
subs01 = subs [2, 3] <> map (1 :) (subs [2, 3])

subs02 :: [[Int]]
subs02 = (subs [3] <> map (2 :) (subs [3])) <> map (1 :) (subs [3] <> map (2 :) (subs [3]))

subs03 :: [[Int]]
subs03 = ((subs [] <> map (3 :) (subs [])) <> map (2 :) (subs [] <> map (3 :) (subs []))) <> map (1 :) ((subs [] <> map (3 :) (subs [])) <> map (2 :) (subs [] <> map (3 :) (subs [])))

subs04 :: [[Int]]
subs04 = (([[]] <> [[3]]) <> map (2 :) ([[]] <> [[3]])) <> map (1 :) (([[]] <> [[3]]) <> map (2 :) ([[]] <> [[3]]))

subs05 :: [[Int]]
subs05 = ([[], [3]] <> map (2 :) [[], [3]]) <> map (1 :) ([[], [3]] <> map (2 :) [[], [3]])

subs06 :: [[Int]]
subs06 = ([[], [3]] <> [[2], [2, 3]]) <> map (1 :) ([[], [3]] <> [[2], [2, 3]])

subs07 :: [[Int]]
subs07 = [[], [3], [2], [2, 3]] <> map (1 :) [[], [3], [2], [2, 3]]

subs08 :: [[Int]]
subs08 = [[], [3], [2], [2, 3]] <> [[1], [1, 3], [1, 2], [1, 2, 3]]

subs09 :: [[Int]]
subs09 = [[], [3], [2], [2, 3], [1], [1, 3], [1, 2], [1, 2, 3]]

{- |

>>> interleave 1 [2,3]
[[1,2,3],[2,1,3],[2,3,1]]
-}
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y : ys) = (x : y : ys) : map (y :) (interleave x ys)

interleave00 :: [[Int]]
interleave00 = interleave 1 [2, 3]
interleave01 :: [[Int]]
interleave01 = [1, 2, 3] : map (2 :) (interleave 1 [3])
interleave02 :: [[Int]]
interleave02 = [1, 2, 3] : map (2 :) ([1, 3] : map (3 :) (interleave 1 []))
interleave03 :: [[Int]]
interleave03 = [1, 2, 3] : map (2 :) ([1, 3] : [3 : [1]])
interleave04 :: [[Int]]
interleave04 = [1, 2, 3] : map (2 :) [[1, 3], [3, 1]]
interleave05 :: [[Int]]
interleave05 = [1, 2, 3] : [[2, 1, 3], [2, 3, 1]]
interleave06 :: [[Int]]
interleave06 = [[1, 2, 3], [2, 1, 3], [2, 3, 1]]

{- |

>>> perms [1,2,3]
[[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
-}
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x : xs) = concat (map (interleave x) (perms xs))

{- | All the possible ways of selecting 0 or more elements in any order.

>>> choices [1,2,3]
[[],[3],[2],[2,3],[3,2],[1],[1,3],[3,1],[1,2],[2,1],[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
-}
choices :: [a] -> [[a]]
choices = concat . map perms . subs


---

{-| Verify a solution is correct.

>>> :{
  let
    e = App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10))
  in solution e [1,3,7,10,25,50] 765
:}
True

-}

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n =
   elem
     (values e)
     (choices ns)
   && eval e == [n]


--- Brute force solution

{-| Returns all possible ways of splitting a list into 2 non-empty lists, lists
that append to give the original list.

>>> split [1,2,3,4]
[([1],[2,3,4]),([1,2],[3,4]),([1,2,3],[4])]
-}
split :: [a] -> [([a], [a])]
split []     = []
split [_]    = []
split (x:xs) = ([x], xs) : [(x:ls, rs) | (ls, rs) <- split xs]

ops :: [Op]
ops = [Add, Sub, Mul, Div]


{-|
>>> combine (Val 1) (Val 2)
[(1+2),(1-2),(1*2),(1/2)]
-}
combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]


{-|

>>> mapM_ print $ exprs [1,2]
(1+2)
(1-2)
(1*2)
(1/2)

>>> mapM_ print $ exprs [1,2,3]
(1+(2+3))
(1-(2+3))
(1*(2+3))
(1/(2+3))
(1+(2-3))
(1-(2-3))
(1*(2-3))
(1/(2-3))
(1+(2*3))
(1-(2*3))
(1*(2*3))
(1/(2*3))
(1+(2/3))
(1-(2/3))
(1*(2/3))
(1/(2/3))
((1+2)+3)
((1+2)-3)
((1+2)*3)
((1+2)/3)
((1-2)+3)
((1-2)-3)
((1-2)*3)
((1-2)/3)
((1*2)+3)
((1*2)-3)
((1*2)*3)
((1*2)/3)
((1/2)+3)
((1/2)-3)
((1/2)*3)
((1/2)/3)

-}
exprs :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns  = [ e
            | (ls,rs) <- split ns
            , l       <- exprs ls
            , r       <- exprs rs
            , e       <- combine l r
            ]


-- mapM_ print $ solutions [1,3,7,10,25,50] 765
solutions :: [Int] -> Int -> [Expr]
solutions ns n =
    [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]


--- Performance

type Result = (Expr, Int)


combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) =
    [ (App o l r, apply o x y)
    | o <- ops
    , valid o x y
    ]

results :: [Int] -> [Result]
results []  = []
results [n] = [ (Val n, n) | n > 0 ]
results ns  = [ res
              | (ls,rs) <- split ns
              , rx      <- results ls
              , ry      <- results rs
              , res     <- combine' rx ry
              ]


-- Roughly 15 times faster than the naive version
-- Then I got *another* 9 times speedup by using `valid` instead of `valid_`
-- (which takes into account the algebraic laws)
solutions' :: [Int] -> Int -> [Expr]
solutions' ns n =
    [ e
    | ns'   <- choices ns
    , (e,m) <- results ns'
    , m == n
    ]

-- Comment out the module keyword at the top to compile as a program:
-- ghc -O2 ./Countdown.hs
main  :: IO ()
main = mapM_ print $ zip [1..] $ solutions' [1,3,7,10,25,50] 765


--- Exercises


-- 1. Redifine `choices` to use list comprehension
{-|

choices = concat . map perms . subs

Simulate concat:
>>> let lst = [[1], [2,3]] in [x | xs <- lst, x <- xs]
[1,2,3]
-}
choices' :: [a] -> [[a]]
choices' xs = [ zs
              | ys <- subs xs
              , zs <- perms ys
              ]


{- 2. Define `isChoice :: Eq a => [a] -> [a] -> Bool` that decides if one
list is chosen from another, without using `perms` and `subs`
-}

removeFirst :: Eq a => a -> [a] -> [a]
removeFirst x xs =
    reverse . snd $
        foldl
            (\(found, lst) n ->
                if not found && n == x then
                    (True, lst)
                else
                    (found, n : lst)
            )
            (False, [])
            xs

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _      = True
isChoice _ []      = False
isChoice (x:xs) ys =
    let zs = removeFirst x ys in
    ys /= zs && isChoice xs zs


{- 3. What effect would generating the function `split` to also return pairs
containing the empty list.

Just extra work, without affecting the final solution as the empty cases would
be filtered out by `exprs`.
-}


{- 4. Using `choices`, `exprs` and `eval`, verify that there are 33,665,406
possible solutions over the numbers [1,3,7,10,25,50].

ghci> length $ exprs =<< choices [1,3,7,10,25,50]
33665406

And that only 4,672,540 of these expressions evaluate successfully

ghci> length $ catMaybes $ fmap eval' (exprs =<< choices [1,3,7,10,25,50])
245644

ghci> length $ filter (/= []) $ fmap eval (exprs =<< choices [1,3,7,10,25,50])
245644

4672540 is obtained if not using the algebraic laws (see `valid_`)

-}

-- Skipping exercises 5 and 6, not very interesting...