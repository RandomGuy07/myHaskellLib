module BasicFunctions
(
    gcd
) where

import Prelude hiding (gcd)

gcd :: (Integral a) => a -> a -> a
gcd a b 
    | b == 0 = a
    | otherwise = gcd b (a `mod` b)