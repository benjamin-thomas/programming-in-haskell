cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

writeAt :: Pos -> String -> IO ()
writeAt p str = do
    goto p
    putStr str

goto :: Pos -> IO ()
goto (x,y) =
    putStr $ "\ESC["
        <> show y
        <> ";"
        <> show x
        <> "H"


width :: Int
width = 10


height :: Int
height = 10


-- Contains living cells only
type Board = [Pos]


glider :: Board
glider =
    [ (4,2)
    , (2,3)
    , (4,3)
    , (3,4)
    , (4,4)
    ]


showCells :: Board -> IO ()
showCells b =
    sequence_ [ writeAt p "0" | p <- b ]


isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b


isEmpty :: Board -> Pos -> Bool
isEmpty b = not . isAlive b


neighbours :: Pos -> [Pos]
neighbours (x,y) =
    map
        wrap
        [ (x-1, y-1), (x, y-1), (x+1, y-1)
        , (x-1, y-0), {-CURR-}  (x+1, y-0)
        , (x-1, y+1), (x, y+1), (x+1, y+1)
        ]


wrap :: Pos -> Pos
wrap (x,y) =
    ( ((x-1) `mod`  width) + 1
    , ((y-1) `mod` height) + 1
    )


liveNeighbours :: Board -> Pos -> Int
liveNeighbours b =
    length . filter (isAlive b) . neighbours


survivors :: Board -> [Pos]
survivors b =
    [ p | p <- b
    , elem (liveNeighbours b p) [2,3]
    ]


birthsNaive :: Board -> [Pos]
birthsNaive b =
    [ (x,y)
    | x <- [1 .. width]
    , y <- [1 .. height]
    , isEmpty b (x,y)
    , liveNeighbours b (x,y) == 3
    ]

births :: Board -> [Pos]
births b =
    [ p | p <- rmdups (concat (map neighbours b))
    , isEmpty b p
    , liveNeighbours b p == 3
    ]


-- I came up with this less efficient version
--rmdups :: Eq a => [a] -> [a]
--rmdups []     = []
--rmdups (x:xs) = x : filter (/= x) (rmdups xs)


-- Book version
rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)


nextGen :: Board -> Board
nextGen b = survivors b <> births b


life :: Board -> IO ()
life b = do
    cls
    showCells b
    wait 500_000
    life (nextGen b)


wait :: Int -> IO ()
wait n =
    sequence_ [ return () | _ <- [1 .. n] ]
