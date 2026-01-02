{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use first" #-}
{-# HLINT ignore "Use const" #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Chapter13.Exercise8 (expr) where

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

a. Translate this description directly into a grammar.
b. implement this grammar as a parser `expr :: Parser Int`
c. What is the problem with this parser?
d. Show how it can be fixed. Hint: rewrite the parser using the repetition
   primitive `many` and the library function `foldl`.

---

This grammar would "associate right", because it recurses on the right.

expr   = term (- expr | ε)
term   = (expr) | int
int    = ... | -1 | 0 | 1 | ...

---

A)

To associate left, we do this:

expr   = expr (- term | ε)
term   = (expr) | int
int    = ... | -1 | 0 | 1 | ...

 -}

{-

B)

>> parse expr "8-7-6"

C) we enter an infinite loop

 -}

-- expr :: Parser Int
-- expr = do
--     n <- expr
--     _ <- sym '-'
--     m <- term
--     return (n - m)

expr :: Parser Int
expr = do
    n <- term
    ns <- many (sym '-' >> term)
    return $ foldl (-) n ns

term :: Parser Int
term =
    parenExpr <|> integer
  where
    parenExpr :: Parser Int
    parenExpr = do
        _ <- sym '('
        e <- expr
        _ <- sym ')'
        return e
