{- |
This provides functions for compiling an AST into a python program that runs the game.
-}
module Compiler (
    CompInfo, applyModifiers, applyRoundModifiers, applyModifier,
    addPhaseToPhaseList, addPhaseToRound, remove0Rounds, updateCompInfo,
    combineCompInfo, getCompRefId, getCompRefIdentifiers, getCompRefIdVals,
    getCompRefIdList, getCompRefs, updateComp, updatePhaseComps, 
    updateRoundComps, updateComps
) where

import AST

-- * Functions for applying modifiers

-- | Applies the modifiers to each round in a game, returning a game with no modifiers
applyModifiers :: Game -> Game
applyModifiers (G p rs wc) = G p (remove0Rounds$ concat $ map (applyRoundModifiers 0 . return) rs) wc

-- | Given a list of rounds, finds the round(s) to which each modifier should apply and applies it
applyRoundModifiers :: Integer -> [Round] -> [Round]
applyRoundModifiers _ rl@((R _ _ []):rs) = rl
applyRoundModifiers m (r@(R pl n ((Jst rn tr pn p):ml)):rs) = if n < rn - m
    then (R pl n ml) : (applyRoundModifiers (n + m) rs)
    else applyRoundModifiers 0 ((applyModifier r m) ++ rs)
applyRoundModifiers m (r@(R pl n ((From rn tr pn p):ml)):rs) = if n < rn - m
    then (R pl n ml) : (applyRoundModifiers (n + m) rs)
    else applyRoundModifiers 0 ((applyModifier r m) ++ (map (addPhaseToRound p tr pn) rs))

-- | Applies a modifier to a round's phase list and returns a list of rounds including the original round and modified round
applyModifier :: Round -> Integer -> [Round]
applyModifier (R pl n ((Jst rn tr pn p):ml)) m = [R pl (rn - m - 1) ml, R (addPhaseToPhaseList pl p tr pn) 1 ml, R pl (n - (rn - m)) ml]
applyModifier (R pl n ((From rn tr pn p):ml)) m = [R pl (rn - m - 1) ml, R (addPhaseToPhaseList pl p tr pn) (n - (rn - m) + 1) ml]

-- | Adds a given phase to a phase list in the position specified by the TimeRef and Number
addPhaseToPhaseList :: [Phase] -> Phase -> TimeRef -> Number -> [Phase]
addPhaseToPhaseList pl newp Before 1 = newp:pl
addPhaseToPhaseList (p:pl) newp After 1 = p:newp:pl
addPhaseToPhaseList (p:pl) newp During 1 = newp:pl
addPhaseToPhaseList (p:pl) newp tr n = if n < 0 || n > toInteger (length pl + 1)
    then error "Phase number is not valid"
    else p : addPhaseToPhaseList pl newp tr (n-1)

-- | Adds a given phase to a given round's phase list and removes the head of the modifier list for that round
addPhaseToRound :: Phase -> TimeRef -> Number -> Round -> Round
addPhaseToRound newp tr n (R pl rep (m:ml)) = R (addPhaseToPhaseList pl newp tr n) rep ml

-- * Functions for updating competitions with more information

-- | Updates all Placed competitions in a Game with information about whether winner/loser information is needed
updateComps :: Game -> Game
updateComps (G p rs wc) = G p (map updateRoundComps rs) wc

-- | Updates all Placed competitions with information about whether winner/loser information is needed in a given Round
updateRoundComps :: Round -> Round
updateRoundComps (R pl n ms) = R (updatePhaseComps pl 1 (getCompRefs pl 0)) n ms 

-- | Updates all Placed competitions in a list of phases with information from the CompInfo
updatePhaseComps :: [Phase] -> Integer -> CompInfo -> [Phase]
updatePhaseComps [] n ci = []
updatePhaseComps (p@(Act (Comp (Scored _ _))):pl) n ci = p : (updatePhaseComps pl (n+1) ci)
updatePhaseComps ((Act (Comp c@(Placed _ il _ _))):pl) n ci = (Act (Comp (updateComp c n ci))) : (updatePhaseComps pl (n+1) ci)
updatePhaseComps (p:pl) n ci = p : (updatePhaseComps pl n ci)

-- | Given a Placed competition and a number representing the competition index with respect to the round, updates the competition with information from the CompInfo
updateComp :: Competition -> Integer -> CompInfo -> Competition
updateComp (Placed c il _ _) n [] = Placed c il False False
updateComp cmp@(Placed c il _ _) n ((num, wn, ln):cis) = if num == n
    then Placed c il wn ln
    else updateComp cmp n cis
updateComp cmp n ci = cmp

-- | Competition information is a number for the index of the competition in the round, a boolean for if it is needed to know the winner, and a boolean for if it is needed to know the loser
type CompInfo = [(Number, Bool, Bool)]

-- | Extracts competition info from a list of phases
getCompRefs :: [Phase] -> Number -> CompInfo
getCompRefs [] _ = []
getCompRefs ((Act (Comp (Scored _ il))):pl) n = combineCompInfo
    (getCompRefIdList il n False) (getCompRefs pl (n+1))
getCompRefs ((Act (Comp (Placed _ il _ _))):pl) n = combineCompInfo
    (getCompRefIdList il n False) (getCompRefs pl (n+1)) 
getCompRefs ((Act (Dec (Vote il1 il2 _))):pl) n = combineCompInfo
    (combineCompInfo (getCompRefIdList il1 n False) (getCompRefIdList il2 n False)) 
    (getCompRefs pl n)
getCompRefs ((Act (Dec (Nomination _ il1 il2 _))):pl) n = combineCompInfo
    (combineCompInfo (getCompRefIdList il1 n False) (getCompRefIdList il2 n False)) 
    (getCompRefs pl n)
getCompRefs ((Act (Dec (Allocation _ il))):pl) n = combineCompInfo  
    (getCompRefIdList il n False) (getCompRefs pl n)
getCompRefs ((Act (Dec (DirectedVote il1 il2 _))):pl) n = combineCompInfo
    (combineCompInfo (getCompRefIdList il1 n False) (getCompRefIdList il2 n False)) 
    (getCompRefs pl n)
getCompRefs ((Act (Dec (Uses id _ _))):pl) n = combineCompInfo 
    (getCompRefId id n False) (getCompRefs pl n)
getCompRefs ((Prog (AU _ il)):pl) n = combineCompInfo (getCompRefIdList il n False) 
    (getCompRefs pl n)
getCompRefs ((Prog (CU _ il)):pl) n = combineCompInfo (getCompRefIdList il n False) 
    (getCompRefs pl n)

-- | Extracts CompInfo from the include and exclude lists of an IdList and combines the CompInfos into a single CompInfo. The boolean argument, if true, signifies that the IdentifierList comes from within a Tiebreaker
getCompRefIdList :: IdentifierList -> Number -> Bool -> CompInfo
getCompRefIdList (IdList idvl il) n t = combineCompInfo (getCompRefIdVals idvl n t) (getCompRefIdentifiers il n t)

-- | Looks through a list of IdentifierVals for references to competition winners or losers, and stores the info in a CompInfo. The boolean argument, if true, signifies that the IdentifierVal list comes from within a Tiebreaker
getCompRefIdVals :: [IdentifierVal] -> Number -> Bool -> CompInfo
getCompRefIdVals [] n t = []
getCompRefIdVals ((IdVal id _):il) n t = combineCompInfo (getCompRefId id n t) (getCompRefIdVals il n t)

-- | Looks through a list of identifiers for references to competition winners or losers, and stores the info in a CompInfo. The boolean argument, if true, signifies that the Identifier list comes from within a Tiebreaker
getCompRefIdentifiers :: [Identifier] -> Number -> Bool -> CompInfo
getCompRefIdentifiers [] n t = []
getCompRefIdentifiers (id:ids) n t = combineCompInfo (getCompRefId id n t) (getCompRefIdentifiers ids n t)

-- | Extracts references to competition winners or losers from identifiers and stores the info in a. The boolean argument, if true, signifies that the Identifier comes from within a Tiebreaker
getCompRefId :: Identifier -> Number -> Bool -> CompInfo
getCompRefId (Winner (CRef num)) n t = if not (t && num == 0)
    then [(compnum, True, False)]
    else []
        where compnum = if num == 0 then n else num
getCompRefId (Loser (CRef num)) n t = if not (t && num == 0) 
    then [(compnum, False, True)]
    else []
        where compnum = if num == 0 then n else num
getCompRefId (Chance il) n t = getCompRefIdList il n t
getCompRefId (Majority _ (Just (Tiebreak _ id))) n t = getCompRefId id n True
getCompRefId (Minority _ (Just (Tiebreak _ id))) n t = getCompRefId id n True
getCompRefId (Most _ il Nothing) n t = getCompRefIdList il n t
getCompRefId (Most _ il (Just (Tiebreak _ id))) n t = combineCompInfo 
    (getCompRefIdList il n t) (getCompRefId id n True)
getCompRefId (Least _ il Nothing) n t = getCompRefIdList il n t
getCompRefId (Least _ il (Just (Tiebreak _ id))) n t = combineCompInfo 
    (getCompRefIdList il n t) (getCompRefId id n True)
getCompRefId id n t = []

-- -- | Sets the boolean in CompInfo for whether a winner is needed to true for a given competition number
-- setWinnerNeeded :: CompInfo -> Number -> CompInfo
-- setWinnerNeeded [] n = [(n, True, False)]
-- setWinnerNeeded (ci@(num, wn, ln):cis) n = if n == num
--     then (num, True, ln):cis
--     else ci : (setWinnerNeeded cis n)

-- -- | Sets the boolean in CompInfo for whether a loser is needed to true for a given competition number
-- setLoserNeeded :: CompInfo -> Number -> CompInfo
-- setLoserNeeded [] n = [(n, False, True)]
-- setLoserNeeded (ci@(num, wn, ln):cis) n = if n == num
--     then (num, wn, True):cis
--     else ci : (setWinnerNeeded cis n)

-- | Combines two CompInfos by updating one CompInfo with each element from the other CompInfo
combineCompInfo :: CompInfo -> CompInfo -> CompInfo
combineCompInfo ci [] = ci
combineCompInfo ci1 (ci:cis) = combineCompInfo (updateCompInfo ci1 ci) cis

-- | Updates a CompInfo with a single CompInfo element by comparing the numbers, performing "or" on the booleans if the Numbers are the same, or simply appending to the CompInfo if it does not contain the Number in the single element
updateCompInfo :: CompInfo -> (Number, Bool, Bool) -> CompInfo
updateCompInfo [] ci = [ci]
updateCompInfo (ci@(num, wn, ln):cis) ci2@(n, wn2, ln2) = if num == n
    then (num, wn || wn2, ln || ln2) : cis
    else ci : (updateCompInfo cis ci2)

-- * Helper Functions

gameRounds :: Game -> [Round]
gameRounds (G _ rs _) = rs

-- | Takes a list of rounds and removes those that occur 0 times
remove0Rounds :: [Round] -> [Round]
remove0Rounds [] = []
remove0Rounds ((R _ 0 _):rs) = remove0Rounds rs
remove0Rounds (r:rs) = r : remove0Rounds rs