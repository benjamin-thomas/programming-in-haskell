module MyLib (hello) where

-- Import the current module beings studied below to make all of its functions accessible via `cabal repl`.
import Chapter05.Caesar

hello :: IO ()
hello = putStrLn "Hello from lib!"
