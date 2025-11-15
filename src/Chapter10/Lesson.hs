import Prelude hiding
    ( getLine
    , putStr
    , putStrLn
    )

act :: IO (Char,Char)
act = do x <- getChar
         getChar
         y <- getChar
         return (x,y)


-- What I came up with, not as elegant as the book version...
getLine' :: IO String
getLine' = do
    x <- getChar
    if x == '\n' then
        return []
    else
        return [x] <> getLine'


getLine :: IO String
getLine = do
    x <- getChar
    if x == '\n' then
        return []
    else do
        xs <- getLine
        return (x : xs)

putStr :: String -> IO ()
putStr []     = return ()
putStr (x:xs) = do
    putChar x
    putStr xs

putStrLn :: String -> IO ()
putStrLn str = do
    putStr str
    putChar '\n'


strlen :: IO ()
strlen = do
    putStr "Enter a string: "
    str <- getLine
    putStrLn $ "The string has " <> show (length str) <> " characters."
