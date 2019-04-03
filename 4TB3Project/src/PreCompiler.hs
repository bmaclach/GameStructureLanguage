{- |
This provides functions for preparing an AST to be further compiled into executable code to run a game.
-}
module PreCompiler (
    CompInfo, applyModifiers, applyRoundModifiers, applyModifier,
    addPhaseToPhaseList, addPhaseToRound, remove0Rounds, updateCompInfo,
    combineCompInfo, getCompRefId, getCompRefIdentifiers, getCompRefIdVals,
    getCompRefIdList, getCompRefs, updateComp, updatePhaseComps, 
    updateRoundComps, updateComps, isNameTaken, getNamesFromPlayerList, 
    getAllNames, getAffsFromPhaseList, getAffsFromRound, getAffsFromAttList, 
    getAffsFromPlayerList, getAllAffiliations, updateTiebreakIds, updateId, 
    updateIdentifierIds, updateIdValIds, updateIdListIds, updateActionIds, 
    updatePhaseIds, updateRoundIds, updateIds, preCompile
) where

import AST
import Data.List (nub)

-- | Updates a Game AST to prepare it for further compilation by applying all modifiers to rounds, updating Placed competitions with information regarding whether the winner or loser must be known, and updating name Identifiers to be affiliation names where appropriate.
preCompile :: Game -> Game
preCompile g = (updateIds . updateComps . applyModifiers) g

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

-- * Functions for updating and checking Identifiers

-- | Changes identifiers with the N constructor to identifiers with the A constructor if the string matches an affiliation and not a player name
updateIds :: Game -> Game
updateIds g@(G pi rl wc) = G pi (map (updateRoundIds playerNames affNames) rl) wc
    where playerNames = getAllNames g
          affNames = getAllAffiliations g playerNames


-- | Changes N-constructed identifiers to A-constructed identifiers if the name is an affiliation for a round
updateRoundIds :: [Name] -> [Name] -> Round -> Round
updateRoundIds pn an (R pl n ms) = R (updatePhaseIds pn an pl) n ms

-- | Changes N-constructed identifiers to A-constructed identifiers if the name is an affiliation for a list of phases
updatePhaseIds :: [Name] -> [Name] -> [Phase] -> [Phase]
updatePhaseIds _ _ [] = []
updatePhaseIds pn an ((Act a):pl) = (Act (updateActionIds pn an a)) : (updatePhaseIds pn an pl)
updatePhaseIds pn an ((Prog (AU au il)):pl) = (Prog (AU au (updateIdListIds pn an il))) : (updatePhaseIds pn an pl)
updatePhaseIds pn an ((Prog (CU cu il)):pl) = (Prog (CU cu (updateIdListIds pn an il))) : (updatePhaseIds pn an pl)

-- | Changes N-constructed identifiers to A-constructed identifiers if the name is an affiliation for an Action
updateActionIds :: [Name] -> [Name] -> Action -> Action
updateActionIds pn an (Comp (Scored c il)) = Comp (Scored c (updateIdListIds pn an il))
updateActionIds pn an (Comp (Placed c il wn ln)) = Comp (Placed c (updateIdListIds pn an il) wn ln)
updateActionIds pn an (Dec (Vote il1 il2 sv)) = Dec (Vote (updateIdListIds pn an il1) (updateIdListIds pn an il2) sv)
updateActionIds pn an (Dec (Nomination n il1 il2 sv)) = Dec (Nomination n (updateIdListIds pn an il1) (updateIdListIds pn an il2) sv)
updateActionIds pn an (Dec (Allocation nm il)) = Dec (Allocation nm (updateIdListIds pn an il))
updateActionIds pn an (Dec (DirectedVote il1 il2 sv)) = Dec (DirectedVote (updateIdListIds pn an il1) (updateIdListIds pn an il2) sv)
updateActionIds pn an (Dec (Uses i ifpl elsepl)) = Dec (Uses (updateId pn an i) (updatePhaseIds pn an ifpl) (updatePhaseIds pn an elsepl))

-- | Changes N-constructed identifiers to A-constructed identifiers if the name is an affiliation for an IdentifierList
updateIdListIds :: [Name] -> [Name] -> IdentifierList -> IdentifierList
updateIdListIds pn an (IdList ivs is) = IdList (updateIdValIds pn an ivs) (updateIdentifierIds pn an is)

-- | Changes N-constructed identifiers to A-constructed identifiers if the name is an affiliation for an IdentifierVal list
updateIdValIds :: [Name] -> [Name] -> [IdentifierVal] -> [IdentifierVal]
updateIdValIds _ _ [] = []
updateIdValIds pn an ((IdVal id val):ivs) = (IdVal (updateId pn an id) val) : (updateIdValIds pn an ivs)

-- | Changes N-constructed identifiers to A-constructed identifiers if the name is an affiliation for an identifier list
updateIdentifierIds :: [Name] -> [Name] -> [Identifier] -> [Identifier]
updateIdentifierIds pn an il = map (updateId pn an) il

-- | Changes N-constructed identifiers to A-constructed identifiers if the name is an affiliation
updateId :: [Name] -> [Name] -> Identifier -> Identifier
updateId pn an (N nm) = if nm `elem` an
    then A nm else if nm `elem` pn
        then N nm 
        else error ("Name " ++ nm ++ " does not match any player or affiliation")
updateId pn an (Chance il) = Chance (updateIdListIds pn an il)
updateId pn an (Majority vr (Just tb)) = Majority vr (Just (updateTiebreakIds pn an tb))
updateId pn an (Minority vr (Just tb)) = Minority vr (Just (updateTiebreakIds pn an tb))
updateId pn an (Most c il Nothing) = Most c (updateIdListIds pn an il) Nothing
updateId pn an (Most c il (Just tb)) = Most c (updateIdListIds pn an il) (Just (updateTiebreakIds pn an tb))
updateId pn an (Least c il Nothing) = Least c (updateIdListIds pn an il) Nothing
updateId pn an (Least c il (Just tb)) = Least c (updateIdListIds pn an il) (Just (updateTiebreakIds pn an tb))
updateId _ _ id = id

updateTiebreakIds :: [Name] -> [Name] -> Tiebreaker -> Tiebreaker
updateTiebreakIds pn an (Tiebreak Nothing id) = Tiebreak Nothing (updateId pn an id)
updateTiebreakIds pn an (Tiebreak (Just a) id) = Tiebreak (Just (updateActionIds pn an a)) (updateId pn an id)

-- | Extracts all player names from a Game
getAllNames :: Game -> [Name]
getAllNames (G (PI pl _ _) _ _) = getNamesFromPlayerList pl

-- | Extracts player names from a list of Players
getNamesFromPlayerList :: [Player] -> [Name]
getNamesFromPlayerList [] = []
getNamesFromPlayerList ((P nm atl):pl) = if nm `elem` (getNamesFromPlayerList pl) 
    then error ("Multiple players named " ++ nm ++ ". Unique names required")
    else nm : (getNamesFromPlayerList pl)

-- | Extracts all affiliation names from a Game and checks to ensure no affiliation names conflict with player names, which are passed in a Name list
getAllAffiliations :: Game -> [Name] -> [Name]
getAllAffiliations (G (PI pl tl _) rl _) pn = if or (map (isNameTaken pn) tl) 
    then error "Player and affiliation have same name. Unique names required."
    else nub $ "nominated" : (getAffsFromPlayerList pl pn ++ tl ++ concat (map ((flip getAffsFromRound) pn) rl))

-- | Extracts affiliations from a player list and stores them in a list, checking to ensure that no affiliation names conflict with player names, which are passed in a Name list
getAffsFromPlayerList :: [Player] -> [Name] -> [Name]
getAffsFromPlayerList [] pn = []
getAffsFromPlayerList ((P _ atl):pl) pn = getAffsFromAttList atl pn ++ getAffsFromPlayerList pl pn

-- | Extracts affiliations from an attribute list and stores them in a list, checking to ensure that no affiliation names conflict with player names, which are passed in a Name list
getAffsFromAttList :: [Attribute] -> [Name] -> [Name]
getAffsFromAttList [] pn = []
getAffsFromAttList ((Affiliation nm):atl) pn = if isNameTaken pn nm
    then error "Player and affiliation have same name. Unique names required."
    else nm : getAffsFromAttList atl pn
getAffsFromAttList (at:atl) pn = getAffsFromAttList atl pn

-- | Extracts affiliations from a round, checking to ensure that no affiliation names conflict with player names, which are passed in a Name list
getAffsFromRound :: Round -> [Name] -> [Name]
getAffsFromRound (R pl _ _) pn = getAffsFromPhaseList pl pn

-- | Extracts affiliations from a phase list, checking to ensure that no affiliation names conflict with player names, which are passed in a Name list
getAffsFromPhaseList :: [Phase] -> [Name] -> [Name]
getAffsFromPhaseList [] _ = []
getAffsFromPhaseList ((Prog (AU (Add nm) _)):pl) pn = if isNameTaken pn nm
    then error "Player and affiliation have same name. Unique names required."
    else nm : (getAffsFromPhaseList pl pn)
getAffsFromPhaseList ((Prog (AU (Change _ nm) _)):pl) pn = if isNameTaken pn nm
    then error "Player and affiliation have same name. Unique names required."
    else nm : (getAffsFromPhaseList pl pn)
getAffsFromPhaseList ((Prog (AU (Swap _ newnms False) _)):pl) pn = if or (map (isNameTaken pn) newnms)
    then error "Player and affiliation have same name. Unique names required."
    else newnms ++ (getAffsFromPhaseList pl pn)
getAffsFromPhaseList ((Prog (AU (Merge _ nm) _)):pl) pn = 
    case nm of Nothing -> "merged" : (getAffsFromPhaseList pl pn)
               Just nm -> if isNameTaken pn nm 
                            then error "Player and affiliation have same name. Unique names required."
                            else nm : (getAffsFromPhaseList pl pn)
getAffsFromPhaseList ((Act (Dec (Uses _ ifpl elsepl))):pl) pn = 
    getAffsFromPhaseList ifpl pn ++ getAffsFromPhaseList elsepl pn
getAffsFromPhaseList (p:pl) pn = getAffsFromPhaseList pl pn

-- * Helper Functions

-- | Takes a list of rounds and removes those that occur 0 times
remove0Rounds :: [Round] -> [Round]
remove0Rounds [] = []
remove0Rounds ((R _ 0 _):rs) = remove0Rounds rs
remove0Rounds (r:rs) = r : remove0Rounds rs

-- | Returns true if the given Name is in the given list of Names
isNameTaken :: [Name] -> Name -> Bool
isNameTaken nl nm = nm `elem` nl