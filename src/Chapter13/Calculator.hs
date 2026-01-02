{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use infix" #-}
module Chapter13.Calculator where

import Chapter13.Lesson (expr, parse)
import Control.Monad (void)
import System.IO (hSetEcho, stdin)
import System.Process (system)

--- UTILS

type Pos = (Int, Int)

writeAt :: Pos -> String -> IO ()
writeAt p str = do
    goto p
    putStr str

goto :: Pos -> IO ()
goto (x, y) =
    putStr $
        "\ESC["
            <> show y
            <> ";"
            <> show x
            <> "H"

getCh :: IO Char
getCh = do
    hSetEcho stdin False
    x <- getChar
    hSetEcho stdin True
    return x

cls :: IO ()
cls = putStr "\ESC[2J"

--- VIEW

box :: [String]
box =
    [ "+---------------+"
    , "|               |"
    , "+---+---+---+---+"
    , "| q | c | d | = |"
    , "+---+---+---+---+"
    , "| 1 | 2 | 3 | + |"
    , "+---+---+---+---+"
    , "| 4 | 5 | 6 | - |"
    , "+---+---+---+---+"
    , "| 7 | 8 | 9 | * |"
    , "+---+---+---+---+"
    , "| 0 | ( | ) | / |"
    , "+---+---+---+---+"
    ]

buttons :: String
buttons = standard ++ extra
  where
    standard = "qcd=123+456-789*0()/"
    extra = "QCD \ESC\BS\DEL\n"

showBox :: IO ()
showBox =
    sequence_
        [ writeAt (1, y) b
        | (y, b) <- zip [1 ..] box
        ]

display :: String -> IO ()
display xs = do
    writeAt (3, 2) (replicate 13 ' ')
    writeAt (3, 2) (reverse (take 13 (reverse xs)))

calc :: String -> IO ()
calc xs = do
    display xs
    c <- getCh
    if elem c buttons
        then do
            clearError
            process c xs
        else do
            beep c xs
            calc xs

clearError :: IO ()
clearError = do
    writeAt (1, 20) (replicate 100 ' ')
    writeAt (1, 21) (replicate 100 ' ')

writeError :: String -> IO ()
writeError str = do
    writeAt (1, 20) ("ERROR: " ++ str)
    writeAt (length str + length "ERROR: ", 21) "^"

beep :: Char -> String -> IO ()
beep c xs = do
    -- putStr "\BEL"
    writeError (xs ++ [c])
    void $ system "paplay /usr/share/sounds/freedesktop/stereo/bell.oga"

process :: Char -> String -> IO ()
process c xs
    | elem c "qQ\ESC" = quit
    | elem c "dD\BS\DEL" = delete xs
    | elem c "=\n" = eval xs
    | elem c "cC" = clear
    | otherwise = press c xs

quit :: IO ()
quit = goto (1, 14)

delete :: String -> IO ()
delete [] = calc []
delete xs = calc (init xs)

eval :: String -> IO ()
eval xs = case parse expr xs of
    [(n, [])] -> calc (show n)
    _ -> do
        beep '?' xs
        calc xs

clear :: IO ()
clear = calc []

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

run :: IO ()
run = do
    cls
    showBox
    clear
