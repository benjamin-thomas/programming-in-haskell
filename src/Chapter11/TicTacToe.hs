{-# OPTIONS_GHC -Wall #-}

-- stack ghci --package pretty-simple --package random ./src/Chapter11/TicTacToe.hs

import Data.Char ( isDigit )

import Data.List
    ( transpose
    , tails
    )

import System.IO
    ( hSetBuffering
    , BufferMode(NoBuffering)
    , stdout
    )

import System.Random ( randomRIO )

size :: Int
size = 3

type Grid = [[Player]]

-- This represents the player's value
data Player
    = O -- The human player
    | B -- Blank
    | X
    deriving
        ( Eq
        , Show

        -- The ordering of the constructor is determined by their position.
        -- Hence: O < B < X
        , Ord
        )


-- This represents the next player move
next :: Player -> Player
next O = X
next X = O
next B = B -- book says we should never call this. FIXME: throw error instead? Or create a sub type?



---------------
-- UTILITIES --
---------------


empty :: Grid
empty = replicate size $ replicate size B

full :: Grid -> Bool
full = all (/= B) . concat

-- Who's turn is it?
turn :: Grid -> Player
turn g =
    if os <= xs then O else X
  where
    os = length $ filter (== O) ps
    xs = length $ filter (== X) ps
    ps = concat g

wins :: Player -> Grid -> Bool
wins p g =
    any line (rows <> cols <> dias)
  where
    line = all (== p)
    rows = g
    cols = transpose g
    dias = [diag g, diag (map reverse g)]


diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size-1]]

diag' :: Grid -> [Player]
diag' g =
    fmap
        (\(i, row) -> row !! i)
        (zip [0..] g)


won :: Grid -> Bool
won g = wins O g || wins X g


-------------
-- Display --
-------------

-- My attempt: first version
putGrid1 :: Grid -> IO ()
putGrid1 g = do
    mapM_
        (\(row,next') -> putStr "  |   |\n"
              >> putPlayers1 row
              >> putStr "\n  |   |"
              >> if null next' then putStr "\n" else putStrLn "\n----------"
        )
        (zip g (tails $ drop 1 g))


-- My attempt: second version
putGrid2 :: Grid -> String
putGrid2 g = concat $
    map
        (\(row,next') -> "  |   |\n"
              ++ putPlayers2 row
              ++ "\n  |   |"
              ++ if null next' then "\n" else "\n----------\n"
        )
        (zip g (tails $ drop 1 g))

-- My attempt: first version
putPlayers1 :: [Player] -> IO ()
putPlayers1 ps =
    case ps of
        []     -> return ()
        (x:xs) -> do
            case x of
                B -> putChar ' '
                O -> putChar 'O'
                X -> putChar 'X'
            if xs == [] then return () else putStr " | "
            putPlayers1 xs


-- My attempt: second version
putPlayers2 :: [Player] -> String
putPlayers2 ps =
    case ps of
        []     -> []
        (x:xs) ->
            let c =
                    case x of
                        B -> ' '
                        O -> 'O'
                        X -> 'X'
            in
            c : if xs == []
                then ' ' : putPlayers2 xs
                else ' ' : '|' : ' ' : putPlayers2 xs


-- Book version
putGrid :: Grid -> IO ()
putGrid =
    putStrLn . unlines . concat . interleave bar . map showRow
  where
    bar = [replicate ((size*4)-1) '-']

showRow :: [Player] -> [String]
showRow =
    beside . interleave bar . map showPlayer
  where
    beside  = foldr1 (zipWith (++))
    bar     = replicate 3 "|"


showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]


interleave :: a -> [a] -> [a]
interleave _ []       = []
interleave _ [x]      = [x]
interleave sep (x:xs) = x : sep : interleave sep xs


-- Making a move

valid :: Grid -> Int -> Bool
valid g i =
    and [ i >= 0
        , i < size*size
        , B == concat g !! i
        ]


-- The singleton list indicates failure (yuck)
move :: Grid -> Int -> Player -> [Grid]
move g i p =
    case splitAt i (concat g) of
        (xs, B:ys) | valid g i ->
            [chop size (xs ++ [p] ++ ys)]
        _ ->
            []


chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)


getNat :: String -> IO Int
getNat prompt' = do
    putStr prompt'
    xs <- getLine
    if xs /= [] && all isDigit xs then
        return (read xs)
    else do
        putStrLn "ERROR: Invalid number"
        getNat prompt'


ticTacToe :: IO ()
ticTacToe = run empty O


cls :: IO ()
cls = putStr "\ESC[2J"


goto :: (Int,Int) -> IO ()
goto (x,y) =
    putStr $ "\ESC["
        <> show y
        <> ";"
        <> show x
        <> "H"

run :: Grid -> Player -> IO ()
run g p = do
    cls
    goto (1,1)
    putGrid g
    run' g p


run' :: Grid -> Player -> IO ()
run' g p
    | wins O g  = putStrLn "Player O wins!\n"
    | wins X g  = putStrLn "Player X wins!\n"
    | full g    = putStrLn "It's a draw!\n"
    | otherwise = do
        i <- getNat (prompt p)
        case move g i p of
            [g'] ->
                run g' (next p)
            _ -> do
                putStrLn "ERROR: Invalid move"
                run' g p


prompt :: Player -> String
prompt p =
    "Player " ++ show p ++ ", enter your move: "


-- GAME TREES


data Tree a
    = Node a [Tree a]
    deriving Show


{-
*Main Text.Pretty.Simple> pPrint $ foldr (\x acc -> Node x [acc]) (Node 4 []) [1,2,3]
Node 1
    [ Node 2
        [ Node 3
            [ Node 4 [] ]
        ]
    ]

*Main Text.Pretty.Simple> pPrint $ foldl (\acc x -> Node x [acc]) (Node 1 []) [2,3,4]
Node 4
    [ Node 3
        [ Node 2
            [ Node 1 [] ]
        ]
    ]

-}

{-

*Main Text.Pretty.Simple> pPrint ( treeFromList [1..4], treeFromList' $ [1..4] )
( Node 1
    [ Node 2
        [ Node 3
            [ Node 4 [] ]
        ]
    ]
, Node 1
    [ Node 2
        [ Node 3
            [ Node 4 [] ]
        ]
    ]
)

-}
treeFromList :: [a] -> Tree a
treeFromList lst =
    case reverse lst of
        []   ->
            error "treeFromList: empty list"
        x:xs ->
            foldl
                (\acc a -> Node a [acc])
                (Node x [])
                xs


treeFromList' :: [a] -> Tree a
treeFromList' lst =
    case reverse lst of
        []   ->
            error "treeFromList': empty list"
        x:xs ->
            foldr
                (\a acc -> Node a [acc])
                (Node x [])
                (reverse xs)

gameTree :: Grid -> Player -> Tree Grid
gameTree g p =
    Node
        g
        [ gameTree g' (next p)
        | g' <- moves g p
        ]


moves :: Grid -> Player -> [Grid]
moves g p
    | won g     = []
    | full g    = []
    | otherwise =
        concat
            [ move g i p
            | i <- [ 0 .. (size * size - 1) ]
            ]


prune :: Int -> Tree a -> Tree a
prune 0 (Node x _)  = Node x []
prune _ (Node x []) = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

-- TODO: pass the printing func!!
printTree :: Show a => Tree a -> IO ()
printTree (Node x []) = print x
printTree (Node x ts) = do
    print x
    mapM_ printTree ts


-- printTree' (pPrintOpt CheckColorTty defaultOutputOptionsDarkBg { outputOptionsCompact = True }) $ prune 3 (gameTree empty O)
printTree' :: Show a => (a -> IO ()) -> Tree a -> IO ()
printTree' printer (Node x []) = printer x
printTree' printer (Node x ts) = do
    printer x
    mapM_ (printTree' printer) ts


-- MINIMAX

{-

Study notes:

Due to `O < B < X`:

  - when it's O's turn, minimum will choose the best score for O (either O or B)
    - it would return B if the children's outcome would mean X wins, or a draw
      - to know that, we have to evaluate the remaining branches, recursively
  - when it's X's turn, maximum will choose the best score for X (either X or B)
    - it would return B if the children's outcome would mean O wins, or a draw
      - to know that, we have to evaluate the remaining branches, recursively

-}
minimax :: Tree Grid -> Tree (Grid,Player)
minimax (Node g []) -- no more moves possible
    | wins O g  = Node (g,O) []
    | wins X g  = Node (g,X) []
    | otherwise = Node (g,B) []
minimax (Node g ts)
-- (min)                                                       v
    | turn g == O = Node (g, minimum ps) ts' -- favor O due to O < B < X
    | turn g == X = Node (g, maximum ps) ts' -- favor X due to O < B < X
-- (max)                                                               ^
  where
    ts' = map minimax ts
    ps  = [p | Node (_,p) _ <- ts']


depth :: Int
depth = 9

bestMove :: Grid -> Player -> IO Grid
bestMove g p = do
    let items = [g' | Node (g',p') _ <- ts, p' == best]
    idx <- randomRIO (0, length items - 1)
    pure $ items !! idx
  where
    tree             = prune depth (gameTree g p)
    Node (_,best) ts = minimax tree


-- Human vs player

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    play empty O


play :: Grid -> Player -> IO ()
play g p = do
    cls
    goto (1,1)
    putGrid g
    play' g p


play' :: Grid -> Player -> IO ()
play' g p
    | wins O g = putStrLn "Player O wins!\n"
    | wins X g = putStrLn "Player X wins!\n"
    | full g   = putStrLn "It's a draw!\n"
    | p == O   = do
        i <- getNat (prompt p)
        case move g i p of
            [] -> do
                putStrLn "ERROR: invalid move"
                play' g p
            [g'] -> play g' (next p)
            --_ -> error "impossible"
    | p == X   = do
        putStr "Player X is thinking... "
        move <- bestMove g p
        (play move) (next p)


-- Exercises

{- 1) Verify that there are 549_946 nodes for a 3×3 tic-tac-toe grid.
      And verify that the maximum depth of the tree is 9.
-}

{-
*Main Text.Pretty.Simple> count $ gameTree empty O
549946
-}
count :: Tree a -> Int
count (Node _ []) = 1
count (Node _ ts) = 1 + sum (fmap count ts)

{-

*Main Text.Pretty.Simple> countDepth $ gameTree empty O
9

-}
countDepth :: Tree a -> Int
countDepth tree =
    aux 0 tree
  where
    aux d (Node _ []) = d
    aux d (Node _ ts) = maximum (fmap (aux (d+1)) ts)

countDepth2 :: Tree a -> Int
countDepth2 (Node _ []) = 0
countDepth2 (Node _ ts) = 1 + maximum (map countDepth2 ts)


{- 2) Choose a random best move

DONE
-}


{-

Skipping exercises 3 & 4, not very interesting...

Appart from alpha-beta pruning wich I should go back to someday.

But I want to move on with the next chapter

-}
