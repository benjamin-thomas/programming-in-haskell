{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use first" #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Chapter13.Lesson where

import Control.Applicative (Alternative (empty, (<|>)), many)
import Data.Char (digitToInt, isAlpha, isAlphaNum, isDigit, isLower, isUpper, toUpper)

-- We can view a Parser as a function going from a String to a structured output
-- type Parser = String -> Tree

-- Because a parser, let's say a number parser, may not consume all the string
-- input, we also return the input we have not consumed.
-- type Parser = String -> (Tree, String)

-- Because a parser may fail, for example a string parser fails on "ABC", we
-- return a list of results, with the convention that the empty list denotes
-- failure, while a singleton list denotes success.
-- Note that we *could* use the list output to propose many possible results.
-- type Parser = String -> [(Tree, String)]

-- Because parsers may return different output types, we parameterize Parser.
-- This type is very similar to `State -> (a, State)` where String is that state
-- except that we can fail the computation by returning an empty list.
-- For this reason, a parser maybe be viewed as a general form of a state
-- transformer.
-- type Parser a = String -> [(a, String)]

-- To allow defining instances, we must use newtype and a type constructor.
newtype Parser a = P (String -> [(a, String)])

-- We define a helper to run the "contained" function.
parse :: Parser a -> String -> [(a, String)]
parse (P p) = p

{-

>>> parse item ""
[]

>>> parse item "abc"
[('a',"bc")]

-}
item :: Parser Char
item =
    P
        ( \s -> case s of
            [] -> []
            x : xs -> [(x, xs)]
        )

-- SEQUENCING PARSERS

-- NOTE: I did't follow the book's instances declarations.
-- Mine ore more "general", they'can handle multiple successful values.

{-

>>> parse (fmap toUpper item) "abc"
[('A',"bc")]

>>> parse (fmap toUpper item) ""
[]

 -}
instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f pa =
        P
            ( \s ->
                fmap
                    (\(a, s') -> (f a, s'))
                    (parse pa s)
            )

{-

>>> parse (pure toUpper <*> item) "abc"
[('A',"bc")]

>>> parse (pure (,) <*> fmap digitToInt item <*> fmap digitToInt item) "abc"
[((10,11),"c")]

>>> parse (pure (+) <*> fmap digitToInt item <*> fmap digitToInt item) "abc"
[(21,"c")]

-}
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

{-

>>> parse three ""
[]

>>> parse three "a"
[]
>>> parse three "ab"
[]

>>> parse three "abc"
[(('a','c'),"")]

>>> parse three "abcd"
[(('a','c'),"d")]

>>> parse three "abcde"
[(('a','c'),"de")]

-}
three :: Parser (Char, Char)
three = pure f <*> item <*> item <*> item
  where
    f x _y z = (x, z)

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    pa >>= f =
        P
            ( \s ->
                concatMap
                    (\(a, s') -> parse (f a) s')
                    (parse pa s)
            )

{-

>>> parse threeAgain ""
[]

>>> parse threeAgain "a"
[]

>>> parse threeAgain "ab"
[]

>>> parse threeAgain "abc"
[(('a','c'),"")]

>>> parse threeAgain "abcd"
[(('a','c'),"d")]

>>> parse threeAgain "abcde"
[(('a','c'),"de")]

-}
threeAgain :: Parser (Char, Char)
threeAgain = do
    x <- item
    _ <- item
    y <- item
    return (x, y)

-- MAKING CHOICES

{-

When we combine parsers with bind, they are applied in sequence: the output
string of the prior parser becomes the input string of the next.

Another way of combining parsers is to apply one parser to the input string, and
if it fails to apply another to the same input string.

Such a concept is captured by the Control.Applicative.Alternative class.

 -}

{-
Commenting out, to obtain "many" for free (further down)

class (Applicative f) => Alternative f where
    empty :: f a -- denotes failure
    (<|>) :: f a -> f a -> f a
-}

{-

>>> parse (empty <|> item) "abc"
[('a',"bc")]

>>> parse (item <|> empty) "abc"
[('a',"bc")]

>>> parse (item <|> return 'D') "abc"
[('a',"bc")]

>>> parse (return 'D' <|> item) "abc"
[('D',"abc")]

>>> parse (empty <|> return 'D') "abc"
[('D',"abc")]

Alternative has 3 laws:

empty <|> x            ≡   x
    x <|> empty        ≡   x
    x <|> (y <|> z)    ≡  (x <|> y) <|> z
 -}
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

{-
Commenting out since importing the real Alternative class creates a conflict

instance Alternative Maybe where
    empty :: Maybe a
    empty = Nothing

    (<|>) :: Maybe a -> Maybe a -> Maybe a
    Just a <|> _ = Just a
    _ <|> b = b
-}

{-

Here's a typical example of Alternative usage over the Maybe type:

>>> Just 1 <|> Just 2 <|> Just 3
Just 1

>>> Nothing <|> Just 2 <|> Just 3
Just 2

>>> Nothing <|> Nothing <|> Just 3
Just 3

>>> Nothing <|> Nothing <|> Nothing
Nothing

 -}

-- DERIVED PRIMITIVES

{-

We now have 3 basic parsers:

- item     : consumes a single char if the string is non-empty
- return v : always succeeds
- empty    : always fails

In combination with sequencing and choice, we can use these 3 primitives to
create other parsers.

 -}
sat :: (Char -> Bool) -> Parser Char
sat p = do
    x <- item
    if p x
        then
            return x
        else
            empty

{- |

>>> parse digit "123hello"
[('1',"23hello")]

>>> parse digit "x123hello"
[]
-}
digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphaNum :: Parser Char
alphaNum = sat isAlphaNum

{- |

>>> parse (char 'a') "abc"
[('a',"bc")]

>>> parse (char 'a') "bcd"
[]
-}
char :: Char -> Parser Char
char x = sat (== x)

{- |

>>> parse (string "hello") "helloWorld"
[("","World")]

>>> parse (string "hello") "hellWorld"
[]
-}

-- string :: String -> Parser String
-- string [] = return []
-- string (x : xs) = do
--     x' <- item
--     if x == x'
--         then
--             string xs
--         else
--             empty

string :: String -> Parser String
string [] = return []
string (x : xs) = do
    _ <- char x
    string xs
