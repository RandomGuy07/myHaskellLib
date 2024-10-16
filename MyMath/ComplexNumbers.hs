module ComlexNumbers
(
    Complex,
    i,
    swapCoordSys,
    conjugate
) where

data Complex a = ComplexCartezian {getReal :: a, getImaginary :: a} | ComplexPolar {getRadius :: a, getAngle :: a}

instance (Ord a, Floating a, Show a) => Show (Complex a) where
    show (ComplexCartezian a b) 
        | ab == b = show a ++ " + i" ++ show b
        | otherwise = show a ++ " - i" ++ show ab
        where ab = abs b
    show (ComplexPolar r fi) = show r ++ " e^i" ++ show fi

instance (Eq a) => Eq (Complex a) where
    (==) (ComplexCartezian a b) (ComplexCartezian c d) = (a == c) && (b == d)
    (==) (ComplexPolar r1 fi1) (ComplexPolar r2 fi2) = (r1 == r2) && (fi1 == fi2)

instance (Floating a) => Num (Complex a) where
    (+) (ComplexCartezian a b) (ComplexCartezian c d) = ComplexCartezian (a+c) (b+d)
    (+) (ComplexPolar r1 fi1) (ComplexPolar r2 fi2) = ComplexPolar r fi where
                                                                            real = r1 * cos fi1 + r2 * cos fi2
                                                                            imag = r1 * sin fi1 + r2 * sin fi2
                                                                            r = sqrt (real * real + imag * imag)
                                                                            fi = atan (imag / real)

    (*) (ComplexCartezian a b) (ComplexCartezian c d) = ComplexCartezian (a*c - b*d) (a*d + b*c)
    (*) (ComplexPolar r1 fi1) (ComplexPolar r2 fi2) = ComplexPolar (r1*r2) (fi1 + fi2)
    
    abs (ComplexCartezian a b) = ComplexCartezian (sqrt (a*a + b*b)) 0
    abs (ComplexPolar r _) = ComplexPolar r 0

    signum (ComplexCartezian a b) = ComplexCartezian (a/r) (b/r) where r = sqrt (a*a + b*b)
    signum (ComplexPolar _ fi) = ComplexPolar 1 fi

    negate (ComplexCartezian a b) = ComplexCartezian (negate a) (negate b)
    negate (ComplexPolar r fi) = ComplexPolar r (fi + pi)

    fromInteger n = ComplexCartezian (fromInteger n) 0



i :: (Floating a) => Complex a
i = ComplexCartezian 0 1

swapCoordSys :: (Floating a) => Complex a -> Complex a
swapCoordSys  (ComplexCartezian a b) = ComplexPolar (sqrt (a*a + b*b)) (atan (b/a))
swapCoordSys (ComplexPolar r fi) = ComplexCartezian (r * cos fi) (r * sin fi)

conjugate :: (Floating a) => Complex a -> Complex a
conjugate (ComplexCartezian a b) = ComplexCartezian a (negate b)
conjugate (ComplexPolar r fi) = ComplexPolar r (pi - fi)