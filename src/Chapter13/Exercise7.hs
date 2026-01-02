{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use first" #-}
{-# HLINT ignore "Use const" #-}

module Chapter13.Exercise7 (expr) where

import Control.Applicative
import Data.Char (isDigit, isSpace)

newtype Parser a = P (String -> [(a, String)])

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f pa =
        P
            ( \s ->
                fmap
                    (\(a, s') -> (f a, s'))
                    (parse pa s)
            )

instance Applicative Parser where
    pure :: a -> Parser a
    pure a = P (\s -> [(a, s)])

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pf <*> pa =
        P
            ( \s ->
                concatMap
                    ( \(ab, s') ->
                        fmap
                            ( \(a, s'') -> (ab a, s'')
                            )
                            (parse pa s') -- [(a, String)]
                    )
                    (parse pf s) -- [(a -> b, String)]
            )

instance Alternative Parser where
    empty :: Parser a
    empty = P (\_ -> [])

    (<|>) :: Parser a -> Parser a -> Parser a
    pa <|> pb =
        P
            ( \s ->
                case parse pa s of
                    [] -> parse pb s
                    x -> x
            )

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    pa >>= f =
        P
            ( \s ->
                concatMap
                    (\(a, s') -> parse (f a) s')
                    (parse pa s)
            )

-- We define a helper to run the "contained" function.
parse :: Parser a -> String -> [(a, String)]
parse (P p) = p

item :: Parser Char
item =
    P
        ( \case
            [] -> []
            x : xs -> [(x, xs)]
        )

sat :: (Char -> Bool) -> Parser Char
sat p = do
    x <- item
    if p x
        then
            return x
        else
            empty

char :: Char -> Parser Char
char x = sat (== x)

space :: Parser ()
space = do
    _ <- many (sat isSpace)
    return ()

token :: Parser a -> Parser a
token p = do
    () <- space
    v <- p
    () <- space
    return v

sym :: Char -> Parser Char
sym = token . char

digit :: Parser Char
digit = sat isDigit

nat :: Parser Int
nat = do
    xs <- some digit
    return (read xs)

int :: Parser Int
int =
    neg <|> nat
  where
    neg =
        char '-' >> negate <$> nat

integer :: Parser Int
integer = token int

{-

CC helped me quite a bit get the grammar right, I don't have a good handle on this...

Before:

expr   = term (+ expr | - expr | ε)
term   = factor (* term | / term | ε)
factor = (expr) | int
int    = ... | -1 | 0 | 1 | ...

New grammar needs to become:

expr   = term (+ expr | - expr | ε)
term   = expo (* term | / term | ε)
expo   = factor (^ expo | ε)
factor = int | (expr)
int    = ... | -1 | 0 | 1 | ...

---

NOTES:

Each grammar line may reference the next grammar line below, *not* above. Except
when hitting an escape hatch, such as the parenthesis here which "reset" precedence.

Also, in this example "A | B" is exactly the same as "B | A" (does not influence
parsing outcome) because A and B are mutually exclusive (we can't have both match).
 -}

expr :: Parser Int
expr = do
    n <- term
    add n <|> sub n <|> return n
  where
    add :: Int -> Parser Int
    add n = do
        _ <- sym '+'
        e <- expr
        return (n + e)

    sub :: Int -> Parser Int
    sub t = do
        _ <- sym '-'
        e <- expr
        return (t - e)

term :: Parser Int
term = do
    n <- expo
    mul n <|> div' n <|> return n
  where
    mul :: Int -> Parser Int
    mul n = do
        _ <- sym '*'
        m <- term
        return (n * m)
    div' :: Int -> Parser Int
    div' n = do
        _ <- sym '/'
        m <- term
        return (n `div` m)

expo :: Parser Int
expo = do
    n <- factor
    exp' n <|> return n
  where
    exp' :: Int -> Parser Int
    exp' n = do
        _ <- sym '^'
        m <- expo
        return (n ^ m)

factor :: Parser Int
factor =
    integer <|> parenExpr
  where
    parenExpr :: Parser Int
    parenExpr = do
        _ <- sym '('
        e <- expr
        _ <- sym ')'
        return e
