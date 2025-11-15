{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ImportQualifiedPost #-}
import Prelude hiding (putStr)
--import Data.Sequence qualified as S
import qualified Streaming.Prelude as S
import Streaming.Prelude (Stream, Of)
--import Data.Sequence (Seq)
import System.IO
    ( hSetEcho
    , stdin
    )


{-

cabal repl --build-depends pretty-simple,streaming
repl> :load Exercise.hs
repl> :load Exercise.hs
repl> readLine
repl> readLineS

-}

{- 1. Define `putStr` using list comprehension and `sequence_` -}
putStr :: String -> IO ()
putStr str =
    sequence_ [ putChar x | x <- str ]


{- 2. Define a version of `putBoard` that displays nim bords of any size. See Nim.hs -}


{- 3. Define `putBoard` using list comprehension + `sequence_` -> already done -}


{- 4. Define an action `adder` that reads a given number of integers from the
keybaord (one per line) and displays their sum -}
adder1 :: IO ()
adder1 = do
    putStr "How many numbers? "
    nums <- read <$> getLine :: IO Int
    putStrLn $ "nums => " <> show nums
    result <- aux nums 0 :: IO Int
    print result
  where
    aux 0 acc = return acc
    aux n acc = fmap read getLine >>= aux (n-1) . (acc+)

-- 5. Same with list comprehension + `sequenc`
adder2 :: IO ()
adder2 = do
    putStr "How many numbers? "
    nums <- read <$> getLine :: IO Int
    result <- sequence [ read <$> getLine | _ <- [1 .. nums] ] :: IO [Int]
    print $ sum result


-- 6. Define `readLine` the behaves like `getLine` but enables deleting chars
getCh :: IO Char
getCh = do
    hSetEcho stdin False
    x <- getChar
    hSetEcho stdin True
    return x

backspace :: IO ()
backspace =
    back >> blank >> back
  where
    back  = putStr "\b"
    blank = putChar ' '

readLine :: IO String
readLine =
    reverse <$> aux []
  where
    aux acc = do
        c <- getCh
        case c of
            '\n'   -> putChar c >> return acc
            '\DEL' -> backspace >> aux (drop 1 acc)
            _      -> putChar c >> aux (c:acc)


{-

6. Try again, but this time with "inspectable data". Think of this being usefull for TDD.

Note: I had to use a streaming library.
It seems impossible to use a list in a lazy way or even `Seq` which represents a *finite* list (unlike OCaml)

-}

data Action
    = NewChar Char
    | BackSpace
    deriving Show


example :: [Action]
example =
    [ NewChar 'H'
    , NewChar 'e'
    , NewChar 'l'
    , NewChar 'l'
    , NewChar 'o'
    , BackSpace
    , BackSpace
    , NewChar 'p'
    ]


-- PRODUCER
genActions :: S.Stream (S.Of Action) IO ()
genActions =
    S.unfoldr
        (\() -> do
            c <- getCh
            return $ case c of
                '\n'   -> Left ()
                '\DEL' -> Right (BackSpace, ())
                _      -> Right (NewChar c, ())
        )
        ()

{- CONSUMER

ghci> readLineS (S.each example)
ghci> readLineS genActions

-}
readLineS :: Stream (Of Action) IO () -> IO String
readLineS stream =
    S.foldM_
        step
        (return [])
        (return . reverse)
        stream
    >>= \str -> putChar '\n' >> return str
  where
    step :: [Char] -> Action -> IO [Char]
    step acc BackSpace   = backspace >> return (drop 1 acc)
    step acc (NewChar c) = putChar c >> return (c:acc)

