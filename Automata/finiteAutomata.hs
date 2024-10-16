import Automata
import Data.List as L


type State = Int
data AutChar = Epsilon | CH Char

-- Finite automata data structures

data DFA = DFA {currentStateDFA :: Maybe State, statesDFA :: [State], gammaDFA :: [Char], deltaDFA :: Char -> State -> Maybe State, startStateDFA :: State, finalStatesDFA :: [State]}
data NFA = NFA {possibleStatesNFA :: [State], statesNFA :: [State], gammaNFA :: [Char], deltaNFA :: Char -> State -> [State], startStateNFA :: State, finalStatesNFA :: [State]}
data ENFA = ENFA {possibleStatesENFA :: [State], statesENFA :: [State], gammaENFA :: [Char], deltaENFA :: Char -> State -> [State], startStateENFA :: State, finalStatesENFA :: [State]}






-- DFA Show and Automata implementation

instance Show DFA where
    show (DFA Nothing _ _ _ _ _) = "Automata failed"
    show (DFA (Just crrSt) _ _ _ _ _) = "q" ++ (show crrSt)

instance Automata DFA where
    isInEndState dfa = (currentStateDFA dfa >>= (myElem $ finalStatesDFA dfa))

    inputChar dfa c = updateStateDFA dfa $ currentStateDFA dfa >>= (deltaDFA dfa) c

-- NFA Show and Automata implementation

instance Show NFA where
    show nfa = (showStateList $ possibleStatesNFA nfa)
               where showStateList [] = []
                     showStateList (x:xs) = "q" ++ show x ++ " " ++ showStateList xs


instance Automata NFA where
    isInEndState nfa = hasSameElementOrdList (possibleStatesNFA nfa) (finalStatesNFA nfa)
    
    inputChar nfa c = updateStateNFA nfa $ removeDuplicateStates $ possibleStatesNFA nfa >>= (deltaNFA nfa) c


-- ENFA Show and Automata implementation

instance Show ENFA where
    show enfa = (showStateList $ possibleStatesENFA enfa)
               where showStateList [] = []
                     showStateList (x:xs) = "q" ++ show x ++ " " ++ showStateList xs


instance Automata ENFA where
    isInEndState enfa = hasSameElementOrdList (possibleStatesENFA enfa) (finalStatesENFA enfa)
    
    inputChar enfa c = updateStateENFA enfa $ removeDuplicateStates $ possibleStatesENFA enfa >>= (deltaENFA enfa) c



--Tranformation function








-- DFA helper functions

deltaList :: [(State,Char,State)] -> Char -> State -> Maybe State
deltaList list c i = findFirst (\(state,char,_) -> c == char && i == state) list >>= maybeGetThird

updateStateDFA :: DFA -> Maybe State -> DFA
updateStateDFA (DFA cst sts gam del strt fin) ns = DFA ns sts gam del strt fin

--NFA helper functions
deltaListNFA :: [(State,Char,[State])] -> Char -> State -> [State]
deltaListNFA list c s = maybeListToList (findFirst (\(state,char,_) -> c == char && s == state) list >>= maybeGetThird)

updateStateNFA :: NFA -> [State] -> NFA
updateStateNFA (NFA cst sts gam del strt fin) ns = NFA ns sts gam del strt fin


--ENFA helper functions

eClosure :: [(State,Char,[State])] -> State -> [State]
eClosure l s = removeDuplicateStates $ aux l [] [s]
               where aux list already mid
                      | mid == [] = already
                      | otherwise = aux list (already `L.union` mid) ((mid \\ already) >>= deltaListNFA list ' ')
            
deltaListENFA :: [(State,Char,[State])] -> Char -> State -> [State]
deltaListENFA list c s = eClosure list s >>= deltaListNFA list c >>= eClosure list

updateStateENFA :: ENFA -> [State] -> ENFA
updateStateENFA (ENFA cst sts gam del strt fin) ns = ENFA ns sts gam del strt fin








-- Test dfa 

testDFA :: DFA
testDFA = DFA (Just 0) [0,1,2] ['0', '1'] (deltaList testFuncDFA) 0 [1]

testFuncDFA :: [(State,Char,State)]
testFuncDFA = [(0,'0',2), (0,'1',0), (1,'0',1), (1,'1',1), (2,'0',2), (2,'1',1)]


-- Test nfa
testNFA :: NFA
testNFA = NFA [0] [0,1,2] ['0','1'] (deltaListNFA testFuncNFA) 0 [2]

testFuncNFA :: [(State,Char,[State])]
testFuncNFA = [(0,'0',[0,1,2]), (0, '1', [0,1,2]), (1, '1', [0,1,2])]

-- Test e-nfa
testENFA :: ENFA
testENFA = ENFA [0] [0,1,2,3,4,5] [' ', '+', '-', '.', '1', '2', '3', '4', '5', '6', '7', '8', '9', '0'] (deltaListENFA testFuncENFA) 0 [5]

testFuncENFA :: [(State,Char,[State])]
testFuncENFA = [(0,' ', [1]), (0, '+', [1]), (0, '-', [1]), (1, '.', [2]), (1, '0', [1,4]), (1, '1', [1,4]), (1, '2', [1,4]), (1, '3', [1,4]), (1, '4', [1,4]), (1, '5', [1,4]), (1, '6', [1,4]), (1, '7', [1,4]), (1, '8', [1,4]), (1, '9', [1,4]), (3, ' ', [5]), (3, '0', [3]), (3, '1', [3]), (3, '2', [3]), (3, '3', [3]), (3, '4', [3]), (3, '5', [3]), (3, '6', [3]), (3, '7', [3]), (3, '8', [3]), (3, '9', [3]), (2, '0', [3]), (2, '1', [3]), (2, '2', [3]), (2, '3', [3]), (2, '4', [3]), (2, '5', [3]), (2, '6', [3]), (2, '7', [3]), (2, '8', [3]), (2, '9', [3]), (4, '.', [3])]




-- helper functions

myElem :: (Eq a) => [a] -> a -> Maybe Bool
myElem list el = Just (el `elem` list)

findFirst :: (a -> Bool) -> [a] -> Maybe a
findFirst f [] = Nothing
findFirst f (x:xs) = if f x then Just x
                            else findFirst f xs

maybeGetThird :: (a,b,c) -> Maybe c
maybeGetThird (x1,x2,x3) = Just x3

maybeListToList :: Maybe [a] -> [a]
maybeListToList Nothing = []
maybeListToList (Just l) = l

removeDuplicateStates :: [State] -> [State]
removeDuplicateStates = (map head) . L.group . L.sort 

hasSameElement :: (Eq a) => [a] -> [a] -> Maybe Bool
hasSameElement [] _ = Just False
hasSameElement (x:xs) l2 
    | x `elem` l2 = Just True
    | otherwise = hasSameElement xs l2


hasSameElementOrdList :: (Ord a) => [a] -> [a] -> Maybe Bool
hasSameElementOrdList [] _ = Just False
hasSameElementOrdList _ [] = Just False
hasSameElementOrdList (x:xs) (y:ys) 
    | x == y = Just True
    | x < y = hasSameElementOrdList xs (y:ys)
    | otherwise = hasSameElementOrdList (x:xs) ys

