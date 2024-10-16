module Rational
(
    Rational,
    makeRational
) where

import Prelude hiding (Rational)

data Rational a = Rational {getNumenator :: a, getDenominator :: a}

instance (Show a) => Show (Rational a) where
    show (Rational a b) = show a ++ "/" ++ show b

instance (Num a, Integral a) => Num (Rational a) where
    (+) (Rational a b) (Rational c d) = reduce $ Rational (a*d + b*c) (b*d)
    (*) (Rational a b) (Rational c d) = reduce $ Rational  (a*c) (b*d)
    negate (Rational a b) = Rational (-a) (b)
    abs (Rational a b) = Rational (abs a) (abs b)
    signum (Rational a b) = Rational ((signum a) * (signum b)) 1
    fromInteger n = Rational (fromInteger n) (fromInteger 1)

instance (Num a, Integral a) => Fractional (Rational a) where
    (/) (Rational a b) (Rational c d) = reduce $ Rational (a*d) (b*c)
    fromRational

reduce :: (Integral a) => Rational a -> Rational a
reduce (Rational a b) = Rational (a `div` den) (b `div` den) 
                        where den = gcd a b

makeRational :: a -> a -> Rational a
makeRational = Rational