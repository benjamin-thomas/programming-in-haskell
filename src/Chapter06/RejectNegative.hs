module Chapter06.RejectNegative where

{- |

>>> fact1 4
24

>>> 1 `div` 0
*** Exception: divide by zero

>>> fact1 (-1)
*** Exception: Input must be non-negative
...


>>> fact2 4
Just 24

>>> fact2 (-1)
Nothing
-}

-- This version does not represent the possible error in the type.
-- It will generate a runtime error
fact1 :: Integer -> Integer
fact1 n
    | n < 0 = error "Input must be non-negative"
    | n == 0 = 1
    | otherwise = n * fact1 (n - 1)

-- This version **does** represent the possible error in the type.
-- It won't generate a runtime error.
-- Plus it remains close to the original, which is quite elegant.
fact2 :: Integer -> Maybe Integer
fact2 n
    | n < 0 = Nothing
    | n == 0 = Just 1
    | otherwise = fmap (* n) (fact2 (n - 1))
