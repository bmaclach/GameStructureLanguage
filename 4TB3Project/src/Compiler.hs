{- |
This provides functions for compiling a Game AST into a Python program that runs the game.
-}
module Compiler (
    IdNames(..), compileAffiliations, compileCounter,
    compileCountersFromAttList, compileCounters, compilePlayer, compilePlayers,
    compileTeam, compileTeams, compilePlayerInfo, compileCompRef,
    compileVoteRef, compileAllocRef, compileValue, compileIdentifier,
    compileIdentifiers, compileIdVal, compileIdVals, compileIdentifierList,
    compileComp, compileDec, compileAction, compileTiebreaker, compileNameList,
    compileAffUpdate, compileCounterUpdate, compileProgression, 
    compilePhaseList, countCompsInPhaseList, countVotesInPhaseList, 
    countAllocsInPhaseList, compileRoundList
) where

import AST
import PreCompiler (getAffsFromAttList)

import Text.PrettyPrint.HughesPJ (Doc, (<+>), (<>), ($$), braces, brackets, 
    colon, comma, doubleQuotes, empty, equals, hcat, integer, nest, parens,
    semi, space, text, vcat)

import Prelude hiding ((<>))
import Data.List (intersperse)
import Control.Monad.Reader

-- | The import of gamelib, which is required in every game
pyImports :: Doc
pyImports = text "from gamelib import *"

-- | The initialization of a Game object, which is required for every game
initGame :: Doc
initGame = text "game" <+> equals <+> text "Game" <> parens empty

-- | The iteration through the round list, which is required for every game
runRounds :: Doc
runRounds = vcat [text "for round in roundList:",
                  nest 4 (text "game.resetResults()"),
                  nest 4 (text "round()")]

-- * Compiling Players

-- | Compiles a PlayerInfo by compiling the players, compiling the teams, and adding a line of python code to randomly divide the players between the teams if the Boolean in the PlayerInfo is True
compilePlayerInfo :: PlayerInfo -> Doc
compilePlayerInfo (PI pl tl rdt) = vcat [
    compilePlayers pl,
    compileTeams tl,
    compileRandomDivide rdt]
    where compileRandomDivide False = empty
          compileRandomDivide True = text "randomlyDivideTeams" <> parens (text "game.teamList" <> comma <+> text "game.playerList")

-- | Compiles a list of team Names into python code for adding the teams to the list of teams
compileTeams :: [Name] -> Doc
compileTeams [] = empty
compileTeams (t:tl) = compileTeam t $$ compileTeams tl

-- | Compiles a single team Name into python code for adding the team to the list of teams
compileTeam :: Name -> Doc
compileTeam nm = text "game.teamList.append" <> parens (doubleQuotes (text nm))

-- | Compiles a list of Players into a series of python instructions for constructing Player objects and adding them to the list of active players
compilePlayers :: [Player] -> Doc
compilePlayers [] = empty
compilePlayers (p:pl) = compilePlayer p $$ compilePlayers pl

-- | Compiles a single Player into python code for constructing a corresponding Player object and adding it to the list of active players
compilePlayer :: Player -> Doc
compilePlayer (P nm atl) = text "game.playerList.append" <> 
    parens (text "Player" <> parens (doubleQuotes (text nm) <> comma <+> 
    compileAffiliations atl <> comma <+> compileCounters
    (compileCountersFromAttList atl)))

-- | Compiles Affiliations from a list of Attributes into a single Doc of a python list of affiliation names
compileAffiliations :: [Attribute] -> Doc
compileAffiliations atl = brackets $ hcat $ intersperse (comma <> space) $ map (doubleQuotes . text) (getAffsFromAttList atl [])

-- | Compiles a list of python dictionary Docs into a single Doc of a python list of dictionaries
compileCounters :: [Doc] -> Doc
compileCounters cs = brackets $ hcat $ intersperse (comma <> space) $ cs

-- | Compiles Counters from a list of Attributes into a list of python dictionary Docs
compileCountersFromAttList :: [Attribute] -> [Doc]
compileCountersFromAttList [] = []
compileCountersFromAttList ((Affiliation _):atl) = compileCountersFromAttList atl
compileCountersFromAttList (c:atl) = (compileCounter c) : (compileCountersFromAttList atl)

-- | Compiles a Counter into a python dictionary Doc
compileCounter :: Attribute -> Doc
compileCounter (Counter nm start minval maxval) = braces $ 
    doubleQuotes (text "counter") <> colon <+> doubleQuotes (text nm) <> 
    startVal start <> ifMin minval <> ifMax maxval
        where startVal Nothing = empty
              startVal (Just i) = comma <+> doubleQuotes (text "starts") <> 
                colon <+> integer i
              ifMin Nothing = empty
              ifMin (Just i) = comma <+> doubleQuotes (text "min") <> 
                colon <+> integer i
              ifMax Nothing = empty
              ifMax (Just i) = comma <+> doubleQuotes (text "max") <> 
                colon <+> integer i
compileCounter a = error "Attempt to compile an Affiliation as a Counter"

-- * Compiling Rounds

-- | Data structure to store all game player names, affiliation names, and counter names
data IdNames = IdNames {
    players :: [Name],
    affs :: [Name],
    counters :: [Name]
}

-- | Compiles a list of rounds into a roundList in python and function definitions for each different type of round.
compileRoundList :: [Round] -> Reader IdNames (Doc, [Doc])
compileRoundList rl = do
    rldoc <- concatRounds rl 1
    return $ (text "roundList =" <+> fst rldoc, snd rldoc)
    where concatRounds :: [Round] -> Integer -> Reader IdNames (Doc, [Doc])
          concatRounds [] _ = return $ (empty, [])
          concatRounds [R pl n ml] num = do
            pldoc <- compilePhaseList pl
            return $ (brackets (text "roundType" <> integer num) <+> text "*" <+> integer n, [vcat [text "def roundType" <> integer num <> parens empty <> colon, nest 4 (fst pldoc)]] ++ snd pldoc)
          concatRounds ((R pl n ml):roundl) num = do
            pldoc <- compilePhaseList pl
            rldoc <- concatRounds roundl (num+1)
            return $ (brackets (text "roundType" <> integer num) <+> text "*" <+> integer n <+> text "+" <+> fst rldoc, [vcat [text "def roundType" <> integer num <> parens empty <> colon, nest 4 (fst pldoc)]] ++ snd rldoc ++ snd pldoc)

-- | Compiles a Phase into python code for the action or progression. The list of Docs is for any function definitions that are required.
compilePhaseList :: [Phase] -> Reader IdNames (Doc, [Doc])
compilePhaseList [] = return $ (empty, [])
compilePhaseList ((Act a):pl) = do
    adoc <- compileAction a
    pldoc <- compilePhaseList pl
    return $ (vcat [fst adoc, fst pldoc], snd adoc ++ snd pldoc)
compilePhaseList ((Prog p):pl) = do
    pdoc <- compileProgression p
    pldoc <- compilePhaseList pl
    return $ (vcat [fst pdoc, fst pldoc], snd pdoc ++ snd pldoc)

-- | Compiles a Progression into python code for updating the affiliation or counter for a given identifier list. The list of Docs is for any function definitions that are required.
compileProgression :: Progression -> Reader IdNames (Doc, [Doc])
compileProgression (AU au il) = do
    ildoc <- compileIdentifierList il 1
    audoc <- compileAffUpdate au
    return $ (vcat [fst ildoc, audoc], snd ildoc)
compileProgression (CU cu il) = do
    ildoc <- compileIdentifierList il 1
    cudoc <- compileCounterUpdate cu
    return $ (vcat [fst ildoc, cudoc], snd ildoc)

-- | Compiles a CounterUpdate into python code for updating a counter.
compileCounterUpdate :: CounterUpdate -> Reader IdNames Doc
compileCounterUpdate (Increase nm v) = do
    ids <- ask
    vdoc <- compileValue v
    if nm `elem` (counters ids) 
        then return $ text "for player in idList1: player.updateCounter" <> parens (vdoc <> comma <+> doubleQuotes (text nm))
        else error ("Reference to non-existent counter " ++ nm)
compileCounterUpdate (Decrease nm v) = do
    ids <- ask
    vdoc <- compileValue v
    if nm `elem` (counters ids) 
        then return $ text "for player in idList1: player.updateCounter" <> parens (text "-" <+> parens vdoc <> comma <+> doubleQuotes (text nm))
        else error ("Reference to non-existent counter " ++ nm)
compileCounterUpdate (Set nm v) = do
    ids <- ask
    vdoc <- compileValue v
    if nm `elem` (counters ids) 
        then return $ text "for player in idList1: player.setCounter" <> parens (vdoc <> comma <+> doubleQuotes (text nm))
        else error ("Reference to non-existent counter " ++ nm)

-- | Compiles an AffiliationUpdate into python code for updating the affiliations. 
compileAffUpdate :: AffiliationUpdate -> Reader IdNames Doc
compileAffUpdate Elimination = return $ text "game.eliminate(idList1)"
compileAffUpdate (Add nm) = return $ text "for player in idList1: player.addAff" <> parens (doubleQuotes (text nm))
compileAffUpdate (Remove nm) = do
    ids <- ask
    if nm `elem` (affs ids)
        then return $ text "for player in idList1: player.removeAff" <> parens (doubleQuotes (text nm))
        else error ("Reference to non-existent affiliation" ++ nm)
compileAffUpdate (Change old new) = do
    ids <- ask
    if old `elem` (affs ids)
        then return $ text "for player in idList1: player.removeAff" <> parens (doubleQuotes (text old)) <> semi <+> text "player.addAff" <> parens (doubleQuotes (text new))
        else error ("Reference to non-existent affiliation " ++ old)
compileAffUpdate (Swap nms newnms np) = do
    ids <- ask
    if and (map (flip elem (affs ids)) nms)
        then return $ text "game.swap" <> parens (compileNameList nms <> comma <+> compileNameList newnms <> comma <+> text "idList1," <+> text (show np))
        else error ("Reference to non-existent affiliation in " ++ show nms)
compileAffUpdate (Merge nms newnm) = do
    ids <- ask
    if and (map (flip elem (affs ids)) nms)
        then return $ text "game.merge" <> parens (compileNameList nms <> comma <+> mergeName newnm <> comma <+> text "idList1") 
        else error ("Reference to non-existent affiliation in " ++ show nms)
    where mergeName Nothing = doubleQuotes $ text "merged"
          mergeName (Just nm) = doubleQuotes $ text nm

-- | Compiles a list of Names into python code for a list of strings.
compileNameList :: [Name] -> Doc
compileNameList nms = brackets $ compileNames nms
    where compileNames [] = empty
          compileNames [x] = doubleQuotes $ text x
          compileNames (x:xs) = doubleQuotes (text x) <> comma <+> compileNames xs

-- | Compiles an Action into the corresponding Competition or Decision python code. The list of Docs is for any function definitions that are required.
compileAction :: Action -> Reader IdNames (Doc, [Doc])
compileAction (Comp c) = compileComp c
compileAction (Dec d) = compileDec d

-- | Compiles a Decision into a call to a python function for getting the decision results. The list of Docs is for any function definitions that are required.
compileDec :: Decision -> Reader IdNames (Doc, [Doc])
compileDec (Vote voterl voteel sv) = do
    voterList <- compileIdentifierList voterl 1
    voteeList <- compileIdentifierList voteel 1
    return $ (vcat [fst voterList, text "voterList = idList1", fst voteeList, text "voteeList = idList1", text "game.vote" <> parens (text "voterList, voteeList," <+> text (show sv))], snd voterList ++ snd voteeList)
compileDec (Nomination n nommers pool sv) = do
    nommerList <- compileIdentifierList nommers 1
    poolList <- compileIdentifierList pool 1
    return (vcat [fst nommerList, text "nommerList = idList1", fst poolList, text "poolList = idList1", text "game.nominate" <> parens (text "nommerList," <+> integer n <> comma <+> text "poolList," <+> text (show sv))], snd nommerList ++ snd poolList)
compileDec (Allocation nm il) = do
    ids <- ask
    ildoc <- compileIdentifierList il 1
    if nm `elem` (counters ids)
        then return (vcat [fst ildoc, text "game.allocate" <> parens (text "idList1," <+> doubleQuotes (text nm))], snd ildoc)
        else error ("Reference to non-existent counter " ++ nm)
compileDec (DirectedVote voterl voteel sv) = do
    voterList <- compileIdentifierList voterl 1
    voteeList <- compileIdentifierList voteel 1
    return $ (vcat [fst voterList, text "voterList = idList1", fst voteeList, text "voteeList = idList1", text "game.directedVote" <> parens (text "voterList, voteeList," <+> text (show sv))], snd voterList ++ snd voteeList)
compileDec (Uses id yespl nopl) = do
    iddoc <- compileIdentifier id 1
    yesdoc <- compilePhaseList yespl
    nodoc <- compilePhaseList nopl
    if nopl == []
        then return $ (vcat [fst iddoc, text "if uses" <> parens (text "ident") <> colon, nest 4 (fst yesdoc), nest 4 (delComps (countCompsInPhaseList yespl)), nest 4 (delVotes (countVotesInPhaseList yespl)), nest 4 (delAllocs (countAllocsInPhaseList yespl))], snd iddoc ++ snd yesdoc)
        else return $ (vcat [fst iddoc, text "if uses" <> parens (text "ident") <> colon,nest 4 (fst yesdoc), nest 4 (delComps (countCompsInPhaseList yespl)), nest 4 (delVotes (countVotesInPhaseList yespl)), nest 4 (delAllocs (countAllocsInPhaseList yespl)), text "else:", nest 4 (fst nodoc), nest 4 (delComps (countCompsInPhaseList nopl)), nest 4 (delVotes (countVotesInPhaseList nopl)), nest 4 (delAllocs (countAllocsInPhaseList nopl))], snd iddoc ++ snd yesdoc ++ snd nodoc) 
        where delComps 0 = empty
              delComps n = text "game.compResults = game.compResults" <> brackets (text "0:len(game.compResults)-" <> integer n)
              delVotes 0 = empty
              delVotes n = text "game.voteResults = game.voteResults" <> brackets (text "0:len(game.voteResults)-" <> integer n)
              delAllocs 0 = empty
              delAllocs n = text "game.allocateResults = game.allocateResults" <> brackets (text "0:len(game.allocateResults)-" <> integer n)

-- | Compiles a Competition into a call to a python function for getting the competition results. The list of Docs is for any function definitions that are required.
compileComp :: Competition -> Reader IdNames (Doc, [Doc])
compileComp (Scored Team il) = do
    ildoc <- compileIdentifierList il 1
    return $ (vcat [fst ildoc, text "game.getScoredTeamCompResults" <> parens (text "list" <> parens (text "dict.fromkeys" <> parens (brackets (text "x for x in game.teamList for y in idList1 if x in y.affiliations"))))], snd ildoc)
compileComp (Scored Individual il) = do
    ildoc <- compileIdentifierList il 1
    return $ (vcat [fst ildoc, text "game.getScoredCompResults" <> parens (text "idList1")], snd ildoc)
compileComp (Placed Team il wn ln) = do
    ildoc <- compileIdentifierList il 1
    return $ (vcat [fst ildoc, text "game.getTeamCompResults" <> parens (text "list" <> parens (text "dict.fromkeys" <> parens (brackets (text "x for x in game.teamList for y in idList1 if x in y.affiliations"))) <> comma <+> text (show wn) <> comma <+> text (show ln))], snd ildoc)
compileComp (Placed Individual il wn ln) = do
    ildoc <- compileIdentifierList il 1
    return $ (vcat [fst ildoc, text "game.getCompResults" <> parens (text "idList1" <> comma <+> text (show wn) <> comma <+> text (show ln))], snd ildoc)    

-- | Compiles an IdentifierList into a python list containing the desired player(s) by filtering out the excludeList from the includeList. The integer input represents the level of nesting, needed to generate unique variable names. The list of Docs is for any function definitions that are required
compileIdentifierList :: IdentifierList -> Integer -> Reader IdNames (Doc, [Doc])
compileIdentifierList (IdList il el) n = do
    inclList <- compileIdVals il n
    exclList <- compileIdentifiers el n
    return $ (vcat [fst inclList, fst exclList, text "idList" <> integer n <+> equals <+> brackets (text "x for x in includeList" <> integer n <+> text "if x not in excludeList" <> integer n)], snd inclList ++ snd exclList)

-- | Compiles a list of IdentifierVals into python code that returns the desired player(s). The integer input represents the level of nesting, needed to generate unique variable names. The returned list of Docs is for any function definitions that are required
compileIdVals :: [IdentifierVal] -> Integer -> Reader IdNames (Doc, [Doc])
compileIdVals idv n = do
    inclList <- concatIdVals idv n
    return $ (vcat [text "includeList" <> integer n <+> equals <+> brackets empty,
              fst inclList], snd inclList)
        where concatIdVals :: [IdentifierVal] -> Integer -> Reader IdNames (Doc, [Doc])
              concatIdVals [] n = return (empty, [])
              concatIdVals (iv:ivs) n = do
                ivdoc <- compileIdVal iv n
                ivsdoc <- concatIdVals ivs n
                return $ (vcat [fst ivdoc, text "includeList" <> integer n <+> text "+= idVal", fst ivsdoc], snd ivdoc ++ snd ivsdoc)

-- | Compiles an IdentifierVal into python code that returns a list containing the Identifier repeated Value times. The integer input represents the level of nesting, needed to generate unique variable names. The returned list of Docs is for any function definitions that are required
compileIdVal :: IdentifierVal -> Integer -> Reader IdNames (Doc, [Doc])
compileIdVal (IdVal id (Num 1)) n = do
    iddoc <- compileIdentifier id n
    return $ (vcat [fst iddoc, text "idVal = ident"], snd iddoc)
compileIdVal (IdVal id v) n = do
    iddoc <- compileIdentifier id n
    vdoc <- compileValue v
    return $ (vcat [
        fst iddoc,
        text "idVal" <+> equals <+> brackets empty,
        text "for player in ident" <> colon <+> text "idVal +=" <+> brackets (text "player") <+> text "*" <+> vdoc], snd iddoc)

-- | Compiles a list of Identifiers into python code that returns the desired player(s). The integer input represents the level of nesting, needed to generate unique variable names. The returned list of Docs is for any function definitions that are required
compileIdentifiers :: [Identifier] -> Integer -> Reader IdNames (Doc, [Doc])
compileIdentifiers il n = do
    exclList <- concatIds il n
    return $ (vcat [text "excludeList" <> integer n <+> equals <+> brackets empty, fst exclList], snd exclList)
        where concatIds :: [Identifier] -> Integer -> Reader IdNames (Doc, [Doc])
              concatIds [] n = return $ (empty, [])
              concatIds (id:idl) n = do
                iddoc <- compileIdentifier id n
                idldoc <- concatIds idl n
                return $ (vcat [fst iddoc, text "excludeList" <> integer n <+> text "+= ident", fst idldoc], snd iddoc ++ snd idldoc)

-- | Compiles an Identifier into python code that returns the desired player(s). The integer input represents the level of nesting, needed to generate unique variable names. The returned list of Docs is for any function definitions that are required
compileIdentifier :: Identifier -> Integer -> Reader IdNames (Doc, [Doc])
compileIdentifier Everyone _ = return $ (text "ident = game.playerList", [])
compileIdentifier (Chance il) n = do
    ildoc <- compileIdentifierList il (n+1)
    return $ (vcat [fst ildoc, text "ident =" <+> brackets (text "randomDraw" <> parens (text "idList" <> integer (n+1)))], snd ildoc)
compileIdentifier Nominated _ = return $ (text "ident =" <+> brackets (text "x for x in game.playerList if" <+> doubleQuotes (text "nominated") <+> text "in x.affiliations"), [])
compileIdentifier Tied _ = return $ (text "ident = tied", [])
compileIdentifier Eliminated _ = return $ (text "ident = eliminated", [])
compileIdentifier (N nm) _ = do
    ids <- ask
    if nm `elem` (players ids)
        then return $ (text "ident =" <+> brackets (text "x for x in game.playerList if x.name ==" <+> doubleQuotes (text nm)), [])
        else error ("Reference to non-existent player" ++ nm) 
compileIdentifier (A af) _ = do
    ids <- ask 
    if af `elem` (affs ids)
        then return $ (text "ident =" <+> brackets (text "x for x in game.playerList if" <+> doubleQuotes (text af) <+> text "in x.affiliations"), [])
        else error ("Reference to non-existent affiliation" ++ af)
compileIdentifier (Winner cr) _ = return $ (text "ident =" <+> brackets (text "x for x in game.playerList if x ==" <+> compileCompRef cr <> brackets (doubleQuotes (text "winner")) <+> text "or" <+> compileCompRef cr <> brackets (doubleQuotes (text "winner")) <+> text "in x.affiliations"), [])
compileIdentifier (Loser cr) _ = return $ (text "ident =" <+> brackets (text "x for x in game.playerList if x ==" <+> compileCompRef cr <> brackets (doubleQuotes (text "loser")) <+> text "or" <+> compileCompRef cr <> brackets (doubleQuotes (text "loser")) <+> text "in x.affiliations"), [])
compileIdentifier (Majority vr Nothing) _ = return $ (text "ident =" <+> brackets (text "getVoteMinOrMax" <> parens (compileVoteRef vr <> comma <+> text "True")), [])
compileIdentifier (Majority vr (Just tb)) n = do
    tbdoc <- compileTiebreaker tb n
    return $ (text "ident =" <+> brackets (text "getVoteMinOrMax" <> parens (compileVoteRef vr <> comma <+> text "True" <> comma <+> fst tbdoc)), snd tbdoc)
compileIdentifier (Minority vr Nothing) _ = return $ (text "ident =" <+> brackets (text "getVoteMinOrMax" <> parens (compileVoteRef vr <> comma <+> text "False")), [])
compileIdentifier (Minority vr (Just tb)) n = do
    tbdoc <- compileTiebreaker tb n
    return $ (text "ident =" <+> brackets (text "getVoteMinOrMax" <> parens (compileVoteRef vr <> comma <+> text "False" <> comma <+> fst tbdoc)), snd tbdoc)
compileIdentifier (Most nm il Nothing) n = do
    ids <- ask
    ildoc <- compileIdentifierList il (n+1)
    if nm `elem` (counters ids) 
        then return $ (vcat [fst ildoc, text "ident =" <+> brackets (text "getMinOrMax" <> parens (text "idList" <> integer (n+1) <> comma <+> doubleQuotes (text nm) <> comma <+> text "True"))], snd ildoc)
        else error ("Reference to non-existent counter " ++ nm)
compileIdentifier (Most nm il (Just tb)) n = do
    ids <- ask
    ildoc <- compileIdentifierList il (n+1)
    tbdoc <- compileTiebreaker tb n
    if nm `elem` (counters ids) 
        then return $ (vcat [fst ildoc, text "ident =" <+> brackets (text "getMinOrMax" <> parens (text "idList" <> integer (n+1) <> comma <+> doubleQuotes (text nm) <> comma <+> text "True" <> comma <+> fst tbdoc))], snd ildoc ++ snd tbdoc)
        else error ("Reference to non-existent counter " ++ nm)
compileIdentifier (Least nm il Nothing) n = do
    ids <- ask
    ildoc <- compileIdentifierList il (n+1)
    if nm `elem` (counters ids) 
        then return $ (vcat [fst ildoc, text "ident =" <+> brackets (text "getMinOrMax" <> parens (text "idList" <> integer (n+1) <> comma <+> doubleQuotes (text nm) <> comma <+> text "False"))], snd ildoc)
        else error ("Reference to non-existent counter " ++ nm)
compileIdentifier (Least nm il (Just tb)) n = do
    ids <- ask
    ildoc <- compileIdentifierList il (n+1)
    tbdoc <- compileTiebreaker tb n
    if nm `elem` (counters ids) 
        then return $ (vcat [fst ildoc, text "ident =" <+> brackets (text "getMinOrMax" <> parens (text "idList" <> integer (n+1) <> comma <+> doubleQuotes (text nm) <> comma <+> text "False" <> comma <+> fst tbdoc))], snd ildoc ++ snd tbdoc)
        else error ("Reference to non-existent counter " ++ nm)


-- | Compiles a Tiebreaker into python code for the name of a function (the first Doc in the output) and the definition of that function (the second Doc in the output). The integer input represents the level of nesting, needed to generate unique variable names.
compileTiebreaker :: Tiebreaker -> Integer -> Reader IdNames (Doc, [Doc])
compileTiebreaker (Tiebreak nm Nothing id) n = do
    iddoc <- compileIdentifier id n
    return $ (text nm <> text "Tiebreaker", [vcat [text "def" <+> text nm <> text "Tiebreaker" <> parens (text "tied") <> colon, nest 4 (fst iddoc), nest 4 (text "return ident[0]")]] ++ snd iddoc)
compileTiebreaker (Tiebreak nm (Just a) id) n = do
    iddoc <- compileIdentifier id n
    adoc <- compileAction a
    return $ (text nm <> text "Tiebreaker", [vcat [text "def" <+> text nm <> text "Tiebreaker" <> parens (text "tied") <> colon, nest 4 (fst adoc), nest 4 (fst iddoc), nest 4 (delResultIfNeeded a), nest 4 (text "return ident[0]")]] ++ snd iddoc ++ snd adoc)
    where delResultIfNeeded (Comp _) = text "del game.compResults[-1]"
          delResultIfNeeded (Dec (Vote _ _ _)) = text "del game.voteResults[-1]"
          delResultIfNeeded (Dec (DirectedVote _ _ _)) = text "del game.voteResults[-1]"
          delResultIfNeeded (Dec (Allocation _ _)) = text "del game.allocateResults[-1]"
          delResultIfNeeded ac = empty


-- | Compiles a Value into python code that returns the desired numeric value
compileValue :: Value -> Reader IdNames Doc
compileValue (Num n) = return $ integer n
compileValue (Count nm) = do
    ids <- ask
    if nm `elem` (counters ids) 
        then return $ text "player.counters" <> brackets (doubleQuotes (text nm))
        else error ("Reference to non-existent counter " ++ nm)
compileValue (Result (Cmp cr)) = return $ compileCompRef cr <> brackets (doubleQuotes (text "scores")) <> brackets (text "player") <+> text "if player in" <+> compileCompRef cr <> brackets (doubleQuotes (text "scores")) <> text ".keys() else" <+> compileCompRef cr <> brackets (doubleQuotes (text "scores")) <> brackets (brackets (text "x for x in game.teamList if x in player.affiliations")<> brackets (text "0")) 
compileValue (Result (Vt vr)) = return $ compileVoteRef vr <> brackets (doubleQuotes (text "votes")) <> brackets (text "player")
compileValue (Result (Alloc ar)) = return $ compileAllocRef ar <> brackets (doubleQuotes (text "allocated")) <> brackets (text "player")

-- | Compiles a CompRef into python code for accessing the compResults array at the referenced index. Note that indexing starts at 1 for the game language.
compileCompRef :: CompRef -> Doc
compileCompRef (CRef num) = text "compResults" <> brackets (integer (num-1))

-- | Compiles a VoteRef into python code for accessing the voteResults array at the referenced index. Note that indexing starts at 1 for the game language.
compileVoteRef :: VoteRef -> Doc
compileVoteRef (VRef num) = text "voteResults" <> brackets (integer (num-1))

-- | Compiles an AllocRef into python code for accessing the allocateResults array at the referenced index. Note that indexing starts at 1 for the game language.
compileAllocRef :: AllocRef -> Doc
compileAllocRef (ARef num) = text "allocateResults" <> brackets (integer (num-1))

-- * Compiling Win Conditions

compileWinCondition

-- * Helper functions

-- | Returns the number of Competitions in a given phase list
countCompsInPhaseList :: [Phase] -> Integer
countCompsInPhaseList [] = 0
countCompsInPhaseList ((Act (Comp _)):pl) = 1 + countCompsInPhaseList pl
countCompsInPhaseList (p:pl) = 0 + countCompsInPhaseList pl

-- | Returns the number of Votes and DirectedVotes in a given phase list
countVotesInPhaseList :: [Phase] -> Integer
countVotesInPhaseList [] = 0
countVotesInPhaseList ((Act (Dec (Vote _ _ _))):pl) = 1 + countVotesInPhaseList pl
countVotesInPhaseList ((Act (Dec (DirectedVote _ _ _))):pl) = 1 + countVotesInPhaseList pl
countVotesInPhaseList (p:pl) = 0 + countVotesInPhaseList pl

-- | Returns the number of Allocations in a given phase list
countAllocsInPhaseList :: [Phase] -> Integer
countAllocsInPhaseList [] = 0
countAllocsInPhaseList ((Act (Dec (Allocation _ _))):pl) = 1 + countAllocsInPhaseList pl
countAllocsInPhaseList (p:pl) = 0 + countAllocsInPhaseList pl