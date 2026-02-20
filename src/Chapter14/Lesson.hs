{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

import Prelude
    ( foldr
    , even
    , map

    , Bool(..)
    , Maybe(..)

    , Eq
    , Ord
    , Show
    , Read

    , Num

    , ($)
    , (+)
    , (*)
    , (.)

    , (++)
    , (&&)
    , (||)
    )

{-

A *monoid* is set together with an associative operator that combines two
elements from the set, and an identity element for the operator

Laws:
mempty `mappend` x  =  x
x `mappend` mempty  =  x
x `mappend` (y `mappend` z)  =  (x `mappend` y) `mappend` z

-}
class Monoid a where
    mempty :: a
    mappend :: a -> a -> a

    mconcat :: [a] -> a
    mconcat = foldr mappend mempty


instance Monoid [a] where
    mempty = []
    mappend = (++)


instance Monoid a => Monoid (Maybe a) where
    mempty = Nothing

    mappend Nothing b = b
    mappend a Nothing = a
    mappend (Just a) (Just b) = Just (mappend a b)


{-

A monoid instance for Int could be defined with either:

- mempty=0 mappend=(+)  -- Combined via addition
- mempty=1 mappend=(*)  -- Combined via multiplication

And since we can only declare a single instance for a given type, we create the
"wrapper" types Sum and Product. This enables us to define unique instances for
each given scenario.

-}
newtype Sum a = Sum a
        deriving
            ( Eq
            , Ord
            , Show
            , Read
            )

getSum :: Sum a -> a
getSum (Sum x) = x


instance Num a => Monoid (Sum a) where
    mempty  = Sum 0
    mappend (Sum a) (Sum b) = Sum (a+b)


newtype Product a = Product a
    deriving
        ( Eq
        , Ord
        , Show
        , Read
        )


getProduct :: Product a -> a
getProduct (Product x) = x


instance Num a => Monoid (Product a) where
    mempty = Product 1
    mappend (Product a) (Product b) = Product (a*b)


{-

We can also define two kind of monoids for Bool:

- one for "conjuction" (&&)    -> All
- one for "disjunction" (||)   -> Any

-}

newtype All = All Bool
    deriving
        ( Eq
        , Ord
        , Show
        , Read
        )


getAll :: All -> Bool
getAll (All x) = x


{-

λ> mconcat $ map (All . even) [2,4,6,8]
All True

λ> mconcat $ map (All . even) [2,3,6,8]
All False

-}
instance Monoid All where
    mempty = All True
    mappend (All a) (All b) = All (a && b)


newtype Any = Any Bool
    deriving
        ( Eq
        , Ord
        , Show
        , Read
        )


getAny :: Any -> Bool
getAny (Any x) = x


{-

λ> mconcat $ map (Any . even) [1,3,5,7]
Any False

λ> mconcat $ map (Any . even) [1,3,6,7]
Any True

-}
instance Monoid Any where
    mempty = Any False
    mappend (Any a) (Any b) = Any (a || b)
