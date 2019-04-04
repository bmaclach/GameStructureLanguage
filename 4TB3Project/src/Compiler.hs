{- |
This provides functions for compiling a Game AST into a Python program that runs the game.
-}
module Compiler (
    IdNames(..), compileAffiliations, compileCounter,
    compileCountersFromAttList, compileCounters, compilePlayer, compilePlayers,
    compileTeam, compileTeams, compilePlayerInfo, compileCompRef,
    compileVoteRef, compileAllocRef, compileValue, compileIdentifier,
    compileIdentifiers, compileIdVal, compileIdVals, compileIdentifierList
) where

import AST
import PreCompiler (getAffsFromAttList)

import Text.PrettyPrint.HughesPJ (Doc, (<+>), (<>), ($$), braces, brackets, 
    colon, comma, doubleQuotes, empty, equals, hcat, integer, parens, space, 
    text, vcat)

import Prelude hiding ((<>))
import Data.List (intersperse)
import Control.Monad.Reader

pyImports :: Doc
pyImports = text "from gamelib import *"

initGame :: Doc
initGame = text "game" <+> equals <+> text "Game" <> parens empty

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
-- compileIdentifier (Majority vr (Just tb)) n = do
--     tbdoc <- compileTiebreaker tb n
--     return $ (text "ident = getVoteMinOrMax" <> parens (compileVoteRef vr <> comma <+> text "True" <> comma <+> fst tbdoc), snd tbdoc)
compileIdentifier (Minority vr Nothing) _ = return $ (text "ident =" <+> brackets (text "getVoteMinOrMax" <> parens (compileVoteRef vr <> comma <+> text "False")), [])
-- compileIdentifier (Minority vr (Just tb)) n = do
--     tbdoc <- compileTiebreaker tb n
--     return $ (text "ident = getVoteMinOrMax" <> parens (compileVoteRef vr <> comma <+> text "False" <> comma <+> fst tbdoc), snd tbdoc)
compileIdentifier (Most nm il Nothing) n = do
    ids <- ask
    ildoc <- compileIdentifierList il (n+1)
    if nm `elem` (counters ids) 
        then return $ (vcat [fst ildoc, text "ident =" <+> brackets (text "getMinOrMax" <> parens (text "idList" <> integer (n+1) <> comma <+> doubleQuotes (text nm) <> comma <+> text "True"))], snd ildoc)
        else error ("Reference to non-existent counter " ++ nm)
-- compileIdentifier (Most nm il (Just tb)) n = do
--     ids <- ask
--     ildoc <- compileIdentifierList il (n+1)
--     tbdoc <- compileTiebreaker tb n
--     if nm `elem` (counters ids) 
--         then return $ (vcat [fst ildoc, text "ident = getMinOrMax" <> parens (text "idList" <> integer (n+1) <> comma <+> text nm <> comma <+> text "True" <> comma <+> fst tbdoc)], snd ildoc ++ snd tbdoc)
--         else error ("Reference to non-existent counter " ++ nm)
compileIdentifier (Least nm il Nothing) n = do
    ids <- ask
    ildoc <- compileIdentifierList il (n+1)
    if nm `elem` (counters ids) 
        then return $ (vcat [fst ildoc, text "ident =" <+> brackets (text "getMinOrMax" <> parens (text "idList" <> integer (n+1) <> comma <+> doubleQuotes (text nm) <> comma <+> text "False"))], snd ildoc)
        else error ("Reference to non-existent counter " ++ nm)
-- compileIdentifier (Least nm il (Just tb)) n = do
--     ids <- ask
--     ildoc <- compileIdentifierList il (n+1)
--     tbdoc <- compileTiebreaker tb n
--     if nm `elem` (counters ids) 
--         then return $ (vcat [fst ildoc, text "ident = getMinOrMax" <> parens (text "idList" <> integer (n+1) <> comma <+> text nm <> comma <+> text "False" <> comma <+> fst tbdoc)], snd ildoc ++ snd tbdoc)
--         else error ("Reference to non-existent counter " ++ nm)


-- | Compiles a Tiebreaker into python code for the name of a function (the first Doc in the output) and the definition of that function (the second Doc in the output). The integer input represents the level of nesting, needed to generate unique variable names.
-- compileTiebreaker :: Tiebreaker -> Integer -> Reader Ids (Doc, [Doc])


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
