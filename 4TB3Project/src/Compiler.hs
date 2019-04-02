{- |
This provides functions for compiling an AST into a python program that runs the game.
-}
module Compiler (
    applyModifiers, applyRoundModifiers, applyModifier, addPhaseToPhaseList, addPhaseToRound, remove0Rounds
) where

import AST

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

-- optimizeComps :: Game -> Game
-- optimizeComps (G p rs wc) = G p (optimizeCompsRounds) wc

-- optimizeCompsRounds :: [Round] -> [Round]
-- optimizeCompsRounds [] = []
-- optimizeCompsRounds ((R ps n ms):rs) = (R (optimizeCompsPhases ps ms) n ms) : optimizeCompsRounds rs

-- optimizeCompsPhases :: [Phase] -> [Modifier] -> [(Int, Bool, Bool)]
-- optimizeCompsPhases 


-- * Helper Functions

gameRounds :: Game -> [Round]
gameRounds (G _ rs _) = rs

-- | Takes a list of rounds and removes those that occur 0 times
remove0Rounds :: [Round] -> [Round]
remove0Rounds [] = []
remove0Rounds ((R _ 0 _):rs) = remove0Rounds rs
remove0Rounds (r:rs) = r : remove0Rounds rs