module Automata
(
    Automata,
    isInEndState,
    inputChar,
    inputWord,
    acceptsWord
) where


class Automata a where
    -- function returns Just True/False if the current state is an end state
    -- returns nothing if automata failed
    isInEndState :: a -> Maybe Bool

    --function that uses a given character on an automata
    inputChar :: a -> Char -> a

    --function that uses a given word on an automata
    inputWord :: a -> String -> a
    inputWord aut [] = aut
    inputWord aut (c:str) = inputWord (inputChar aut c) str

    --function that returns Just True/False if automata accepted given word
    --returns nothing if automata failed given a word
    acceptsWord :: a -> String -> Maybe Bool
    acceptsWord aut = isInEndState . inputWord aut

