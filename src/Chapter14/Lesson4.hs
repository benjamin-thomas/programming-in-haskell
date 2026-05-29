module Chapter14.Lesson4 where

-- Traversable

{-

Reminder: mapping over a list

>>> map_ (*2) [1..3]
[2,4,6]

 -}
{- HLINT ignore "Use map" -}
{- HLINT ignore "Use foldr" -}
map_ :: (a -> b) -> [a] -> [b]
map_ _ [] = []
map_ f (x : xs) = f x : map_ f xs

{-

Rather then applying (a -> b), suppose we would rather apply (a -> Maybe b) to signal failure.

We could call such a function "traverse".

Because Maybe is Applicative:

>>> pure (*) <*> Just 3 <*> Just 4
Just 12

>>> (*) <$> Just 3 <*> Just 4
Just 12

We can define traverse_ as such:

>>> traverse_ (\x -> if x <= 0 then Nothing else Just x) [1,2,3]
Just [1,2,3]

>>> traverse_ (\x -> if x <= 0 then Nothing else Just x) [1,-2,3]
Nothing

In summary, "traverse_" here provides a simple means of traversing the elements
of a list using a function that may fail.
 -}
traverse_ :: (a -> Maybe b) -> [a] -> Maybe [b]
traverse_ _ [] = Just []
traverse_ f (x : xs) = (:) <$> f x <*> traverse_ f xs

{-

The idea of traversing a data structure isn't specific to lists, and isn't
specific to functions that may fail.

Tha class of types that support such a generalized mapping are known as
"traversable types", or "traversables" for short.

 -}