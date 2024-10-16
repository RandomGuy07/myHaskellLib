
-- A class representing a semigroup for "addition"
-- The plus function should hold the asociativity law:
-- plus (plus a b) c = plus a (plus b c)
class SemigroupAdd a where
    plus :: a -> a -> a

-- A class representing a monoid for "addition"
-- The element unitplus should have the folowing property for any element a:
-- plus unitplus a = plus a unitplus = a
class (SemigroupAdd a) => MonoidAdd a where
    unitplus :: a

-- A class representing a group for "addition"
-- The function inverseplus should return the inverse element of a for the "addition operation"
-- plus (inverseplus a) a = plus a (inverseplus a) = unitplus
class (MonoidAdd a) => GroupAdd a where
    inveresplus :: a -> a


-- A class representing a semigroup for "multiplication"
-- The mult function should hold the asociativity law:
-- mult (mult a b) c = mult a (mult b c)
class SemigroupMult a where
    mult :: a -> a -> a

-- A class representing a monoid for "multiplication"
-- The element unitmult should have the folowing property for any element a:
-- mult unitmult a = mult a unitmult = a
class (SemigroupMult a) => MonoidMult a where
    unitmult :: a

-- A class representing a group for "multiplication"
-- The function inversemult should return the inverse element of a for the "multiplication operation"
-- mult (inversemult a) a = mult a (inversemult a) = unitmult
class (MonoidMult a) => GroupMult a where
    inversemult :: a -> a

