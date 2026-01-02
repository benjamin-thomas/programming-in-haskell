{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use const" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use first" #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Chapter13.Lesson (expr, parse) where

import Control.Applicative ()
import Data.Char (digitToInt, isAlpha, isAlphaNum, isDigit, isLower, isSpace, isUpper, toUpper)

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

class (Applicative f) => Alternative f where
    empty :: f a -- denotes failure
    (<|>) :: f a -> f a -> f a

    -- this is nuts
    many :: f a -> f [a]
    many x = some x <|> pure []

    some :: f a -> f [a]
    some x = pure (:) <*> x <*> many x

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

instance Alternative Maybe where
    empty :: Maybe a
    empty = Nothing

    (<|>) :: Maybe a -> Maybe a -> Maybe a
    Just a <|> _ = Just a
    _ <|> b = b

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
[("hello","World")]

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
    _ <- string xs
    return (x : xs)

{- |

>>> parse ident "Hello123"
[]

>>> parse ident "hello123 = 9"
[("hello123"," = 9")]
-}
ident :: Parser String
ident = do
    x <- lower
    xs <- many alphaNum
    return (x : xs)

{- |

>>> parse nat "abc"
[]

>>> parse nat "123abc"
[(123,"abc")]
-}
nat :: Parser Int
nat = do
    xs <- some digit
    return (read xs)

{- |

>>> parse space "123abc"
[((),"123abc")]

>>> parse space "   123abc"
[((),"123abc")]
-}
space :: Parser ()
space = do
    _ <- many (sat isSpace)
    return ()

{- |

>>> parse int "123"
[(123,"")]

>>> parse int "-123"
[(-123,"")]

>>> parse int "--123"
[]
-}
int :: Parser Int
int =
    neg <|> nat
  where
    neg =
        char '-' >> negate <$> nat

-- HANDLING SPACING

{- | Applies a parser, ignoring spaces around the desired value


>>> parse (token int) "  123  45  "
[(123,"45  ")]
-}
token :: Parser a -> Parser a
token p = do
    () <- space
    v <- p
    () <- space
    return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

{- |

>>> parse (symbol "var") "   var x = 1"
[("var","x = 1")]
-}
symbol :: String -> Parser String
symbol = token . string

sym :: Char -> Parser Char
sym = token . char

{-

>>> parse nats "[1,2 , 3   ,4]"
[([1,2,3,4],"")]

>>> parse nats "[1,2 , 3   ,4"
[]

 -}
nats :: Parser [Int]
nats = do
    x <- sym '[' >> natural
    xs <- many (sym ',' >> natural)
    _ <- sym ']'
    return (x : xs)

-- ARITHMETIC EXPRESSIONS

expr :: Parser Int
expr = do
    t <- term
    add t <|> sub t <|> return t
  where
    add :: Int -> Parser Int
    add t = do
        _ <- sym '+'
        e <- expr
        return (t + e)

    sub :: Int -> Parser Int
    sub t = do
        _ <- sym '-'
        e <- expr
        return (t - e)

term :: Parser Int
term = do
    f <- factor
    mul f <|> div' f <|> return f
  where
    mul :: Int -> Parser Int
    mul f = do
        _ <- sym '*'
        t <- term
        return (f * t)
    div' :: Int -> Parser Int
    div' f = do
        _ <- sym '/'
        t <- term
        return (f `div` t)

factor :: Parser Int
factor =
    parenExpr <|> integer
  where
    parenExpr :: Parser Int
    parenExpr = do
        _ <- sym '('
        e <- expr
        _ <- sym ')'
        return e

{-

>>> eval "2+3*4"
14

>>> eval "2*3+4"
10
 -}
eval :: String -> Int
eval xs = case parse expr xs of
    [(n, [])] -> n
    [(_, out)] -> error ("Unused input: " ++ out)
    _ -> error "Invalid input"

-- CALCULATOR

-- See Calculator.hs

--- EXERCISES

{-

1) Define a Haskell comment parser

>>> parse comment "-- Hello\nLINE2"
[((),"LINE2")]

 -}
comment :: Parser ()
comment = do
    _ <- string "--"
    _ <- many (sat (/= '\n'))
    _ <- char '\n'
    return ()

{-

2) Using our second grammar for arithmetic expressions, draw the two possible
parse trees for the expression 2+3+4

expr   = expr + expr | term
term   = term * term | factor
factor = (expr) | nat
nat    = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

---

A)

expr
├─ expr ─ term ─ factor ─ nat 2
├─ +
└─ expr
    ├─ expr ─ term ─ factor ─ nat 3
    ├─ +
    └─ expr ─ term ─ factor ─ nat 4

---

B)

expr
├─ expr
│  ├─ expr ─ term ─ factor ─ nat 2
│  ├─ +
│  └─ expr ─ term ─ factor ─ nat 3
├─ +
└─ expr ─ term ─ factor ─ nat 4

 -}

{-

3) Using our third grammar, draw the parse tree for the following expressions:

- 2+3
- 2*3*4
- (2+3)+4

  expr   = term   (+ expr | ε)
  term   = factor (* term | ε)
  factor = (expr) | nat

---

2+3

expr
├─ term ─ factor ─ nat ─ 2
├─ +
└─ expr ─ term ─ factor ─ nat 3

---

2*3

expr
└─ term
    ├─ factor ─ nat ─ 2
    ├─ *
    └─ term ─ factor ─ nat ─ 3

---

2*3*4

expr
└─ term
    ├─ factor ─ nat ─ 2
    ├─ *
    └─ term
         ├─ factor ─ nat ─ 3
         ├─ *
         └─ term ─ factor ─ nat ─ 4

---

(2+3)+4

expr
├─ term ─ factor
│           ├─ (
│           ├─ expr
│           │    ├─ term ─ factor ─ nat ─ 2
│           │    ├─ +
│           │    └─ expr ─ term ─ factor ─ nat ─ 3
│           │
│           └─ )
├─ +
└─ expr ─ term ─ factor ─ nat ─ 4

--- Better trees draw by chatgpt (verifying my answers)

2+3

expr
├─ term
│  └─ factor
│     └─ nat(2)
├─ '+'
└─ expr
   └─ term
      └─ factor
         └─ nat(3)

---

2*3*4 (right associative)

expr
└─ term
   ├─ factor
   │  └─ nat(2)
   ├─ '*'
   └─ term
      ├─ factor
      │  └─ nat(3)
      ├─ '*'
      └─ term
         └─ factor
            └─ nat(4)

---

(2+3)+4

expr
├─ term
│  └─ factor
│     ├─ '('
│     ├─ expr
│     │  ├─ term
│     │  │  └─ factor
│     │  │     └─ nat(2)
│     │  ├─ '+'
│     │  └─ expr
│     │     └─ term
│     │        └─ factor
│     │           └─ nat(3)
│     └─ ')'
├─ '+'
└─ expr
   └─ term
      └─ factor
         └─ nat(4)

 -}

{-

4) Explain why the final simplification of the grammar has a dramatic effect on
the efficiency of the parser.

Hint: begin by considering how an expression comprising a single number would be
parsed if this simplification step had not been made.

For each expr and term node in the tree, we'd do the work twice. Besides it
takes many steps to obtain the right side result.

Parse 0:
    try term + expr
        try factor * term
        backtrack
    backtrack
        try term
            try factor * term
            backtrack
            try factor
                OK

Compared to:

Parse 0:
    term
        try "+ expr" -> fails
        try factor
            try "* term" -> fails
                try "(expr)" -> fails
                    try nat 0 -> success

 -}

{-

5) Define a type for Expr and modify the parser for expressions to have type:

expr :: Parser Expr

-}

data Expr
    = Add Term Expr
    | Ter Term
    deriving (Show)

data Term
    = Mul Factor Term
    | Fac Factor
    deriving (Show)

data Factor
    = Prn Expr
    | Nat Int
    deriving (Show)

{-

>>> parse expr' "2+3*4"
[(Add (Fac (Nat 2)) (Ter (Mul (Nat 3) (Fac (Nat 4)))),"")]
-}
expr' :: Parser Expr
expr' = do
    t <- term'
    add t <|> return (Ter t)
  where
    add :: Term -> Parser Expr
    add t = do
        _ <- sym '+'
        e <- expr'
        return $ Add t e

{-

>>> parse term' "1"
[(Fac (Nat 1),"")]

>>> parse term' "2*3"
[(Mul (Nat 2) (Fac (Nat 3)),"")]
-}
term' :: Parser Term
term' = do
    f <- factor'
    mul f <|> return (Fac f)
  where
    mul :: Factor -> Parser Term
    mul f = do
        _ <- sym '*'
        t <- term'
        return $ Mul f t

{-

>>> parse factor' "1"
[(Nat 1,"")]

>>> parse factor' "(1)"
[(Prn (Ter (Fac (Nat 1))),"")]

>>> parse factor' "(1+2)"
[(Prn (Add (Fac (Nat 1)) (Ter (Fac (Nat 2)))),"")]

 -}
factor' :: Parser Factor
factor' =
    parenExpr <|> nat'
  where
    nat' :: Parser Factor
    nat' = Nat <$> natural

    parenExpr :: Parser Factor
    parenExpr = do
        _ <- sym '('
        e <- expr'
        _ <- sym ')'
        return (Prn e)

{-

6) Extend the parser `expr :: Parser Int` to support subtraction and division,
and to use integer values rather than natural numbers based upon the following
revision of the grammar.

expr   = term (+ expr | - expr | ε)
term   = factor (* term | / term | ε)
factor = (expr) | int
int    = ... | -1 | 0 | 1 | ...

 -}

{-

7) Further extend the grammar to support exponentiation which is assumed to
associate to the right, and have higher priority than multiplication and
division, but lower priority than parentheses and numbers.

For example, 2^3*4 means (2^3)*4

Hint: the new level of priority requires a new rule in the grammar.

---

New grammar needs to become:

expr   = term (+ expr | - expr | ε)
term   = expo (* term | / term | ε)
expo   = factor (^ expo | ε)
factor = int | (expr)
int    = ... | -1 | 0 | 1 | ...

See Exercise7.hs

-}

{-

8) Consider expressions built from natural numbers using a subtraction operator
that is assumed to associate on the left.

a. Translate this description directly into a grammar.
b. implement this grammar as a parser `expr :: Parser Int`
c. What is the problem with this parser?
d. Show how it can be fixed. Hint: rewrite the parser using the repetition
   primitive `many` and the library function `foldl`.

See Exercise8.hs

 -}

{-

9) Modify the calculator program to indicate the approximate position of an error
rather than just sounding a beep.

DONE

-}