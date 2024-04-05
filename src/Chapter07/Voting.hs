{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Chapter07.Voting where

import Data.List (sort)

-- echo ./src/Chapter07/Voting.hs | entr -c doctest /_

-------------------------------------------------------------------------------
-- FIRST PAST THE POST ALGORITHM
-------------------------------------------------------------------------------

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

{- |

We could implement the following functions with recursion, but using
higher-order functions is simpler.

>>> filter (== "Red") votes
["Red","Red"]

>>> length $ filter (== "Red") votes
2

>>> count "Red" votes
2
-}
count :: (Eq a) => a -> [a] -> Int
count x = length . filter (== x)

{- |

>>> uniq votes
["Red","Blue","Green"]
-}
uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq (x : xs) = x : filter (/= x) (uniq xs)

{- |

>>> result votes
[(1,"Green"),(2,"Red"),(3,"Blue")]
-}
result :: (Ord a) => [a] -> [(Int, a)]
result vs = sort [(count v vs, v) | v <- uniq vs]

{- |

>>> last $ result votes
(3,"Blue")

>>> snd $ last $ result votes
"Blue"

>>> snd . last . result $ votes
"Blue"
-}
winner :: (Ord a) => [a] -> a
winner = snd . last . result

-------------------------------------------------------------------------------
-- ALTERNATIVE VOTE ALGORITHM
-------------------------------------------------------------------------------

-- In this voting system, each person can vote for as many or as few candidates
-- as they wish.

ballots :: [[String]]
ballots =
    [ ["Red", "Green"]
    , ["Blue"]
    , ["Green", "Red", "Blue"]
    , ["Blue", "Green", "Red"]
    , ["Green"]
    ]

step1 =
    -- Red is eliminated as it has the smallest number of occurrences in first position
    [ ["Green"]
    , ["Blue"]
    , ["Green", "Blue"]
    , ["Blue", "Green"]
    , ["Green"]
    ]

step2 =
    -- Now Blue is eliminated based on the result of step1
    -- Green won
    [ ["Green"]
    , []
    , ["Green"]
    , ["Green"]
    , ["Green"]
    ]

rmEmpty :: (Eq a) => [[a]] -> [[a]]
rmEmpty = filter (not . null)

{- |

>>> eliminate "Blue" $ eliminate "Red" ballots
[["Green"],[],["Green"],["Green"],["Green"]]

>>> rank $ rmEmpty $ [["Green"],[],["Green"],["Green"],["Green"]]
["Green"]
-}
eliminate :: (Eq a) => a -> [[a]] -> [[a]]
eliminate x = map (filter (/= x))

{- |

>>> result ballots
[(1,["Blue"]),(1,["Blue","Green","Red"]),(1,["Green"]),(1,["Green","Red","Blue"]),(1,["Red","Green"])]

---

Step 1: eliminate red

>>> map head ballots
["Red","Blue","Green","Blue","Green"]

>>> result $ map head ballots
[(1,"Red"),(2,"Blue"),(2,"Green")]

>>> map snd $ result $ map head ballots
["Red","Blue","Green"]

>>> map snd . result . map head $ ballots
["Red","Blue","Green"]

>>> rank ballots
["Red","Blue","Green"]
-}
rank :: (Ord a) => [[a]] -> [a]
rank = map snd . result . map head

{- |

>>> winner' ballots
"Green"
-}
winner' :: (Ord a) => [[a]] -> a
winner' ballots = case rank (rmEmpty ballots) of
    [x] -> x
    (candidate : _) -> winner' (eliminate candidate ballots)
