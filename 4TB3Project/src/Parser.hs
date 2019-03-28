{-# LANGUAGE FlexibleContexts #-}

{- |
This provides functions for parsing a string written in this language into an AST
-}
module Parser (
    parseGame, teamList, team, playerList, player, attributeList, attribute, 
    affiliation, counter, roundReference, phaseReference, timeReference,
    compReference, voteReference, allocateReference, actReference, value,
    identifierList, idValList, idList, identifierVal, identifierP
) where

import Text.Parsec
import Scanner
import AST
import Data.Functor.Identity

-- | This takes a string representation of a game, parses it into an AST, and returns the AST, or an error if one occurs.
parseGame :: (Stream s Identity t) => Parsec s () a -> s -> a
parseGame parser gametoparse = case (parse parser "" gametoparse) of
    Left e -> error $ show e
    Right ast -> ast

-- * Player-related parsers

-- | Parser for a team list, which can be either a list of teams and their associated players, a list of players (if there are no teams), or a list of players and teams and an instruction to randomly divide the players between the teams
teamList = do 
    everyone <- teamTeamList <|> teamPlayerList <|> teamRandomList
    al <- option [] (do {reserved "all"
                       ; reserved "with"
                       ; attList <- attributeList
                       ; return attList
                    })
    return $ addAttributesPI al everyone

teamTeamList = do 
    tl <- sepBy1 team semi
    return $ PI (concat $ map fst tl) (concat $ map snd tl) False

teamPlayerList = do
    pl <- playerList
    return $ PI pl [] False

teamRandomList = do
    reserved "randomly"
    reserved "divide"
    pl <- playerList
    reserved "into"
    tl <- commaSep identifier
    return $ PI pl tl True

-- | Parser for a team, returns a pair of the players in the team and the team name
team = do
    reserved "team"
    teamNm <- identifier
    colon
    pl <- playerList
    return $ (map (addAttribute (Affiliation teamNm)) pl, [teamNm])

-- | Parser for a list of players
playerList = commaSep player

-- | Parser for a player
player = do
    nm <- identifier
    al <- option [] (do {reserved "with"
                        ; attList <- attributeList
                        ; return attList
                    })
    return $ P nm al

-- | Parser for a list of attributes
attributeList = commaSep attribute

-- | Parser for attributes, returns an Attribute AST node
attribute = affiliation <|> counter

-- | Parser for affiliations, returns an Attribute AST node
affiliation = do
    reserved "affiliation"
    reserved "called"
    aff <- identifier
    return $ Affiliation aff

-- | Parser for counters, returns an Attribute AST node
counter = do
    (reserved "score" <|> reserved "resource" <|> reserved "counter")
    reserved "called"
    nm <- identifier
    start <- optionMaybe (do {reserved "starting"
                             ; reserved "at"
                             ; num <- number
                             ; return num
                             })
    min <- optionMaybe (try (do {reserved "with"
                           ; reserved "minimum"
                           ; reserved "of"
                           ; num <- number
                           ; return num
                           }))
    max <- optionMaybe (do {reserved "with"
                           ; reserved "maximum"
                           ; reserved "of"
                           ; num <- number
                           ; return num
                           })
    return $ Counter nm start min max

-- * Round-related parsers

-- | Parser for competitors, either team or individual
competitor = option Individual (do {reserved "team"
                                   ; return Team})

-- | Parser for identifier lists, which are defined by a list of identifiers to include and a list of identifiers to exclude
identifierList = do
    idv <- idValList
    idl <- option [] (do {reserved "except"
                         ; il <- idList
                         ; return il})
    return $ IdList idv idl

-- | Parser for simple comma-separated lists of identifier-value pairs
idValList = commaSep identifierVal

-- | Parser for simple comma-separated lists of identifiers
idList = commaSep identifierP

-- | Parser for identifiers paired with a value. If no value is specified, 1 is used by default.
identifierVal = do
    ident <- identifierP
    val <- option (Num 1) (do {reservedOp "*"
                        ; v <- value
                        ; return v})
    return $ IdVal ident val

-- | Parser for identifiers, which can be everyone, chance, nominated, tied, eliminated, a name, the winner or loser of a competition, the majority or minority of a vote, or the player with the highest or lowest value of a counter
identifierP = do {reserved "everyone"
                ; return Everyone}
             <|> do {reserved "chance"
                    ; il <- parens identifierList
                    ; return $ Chance il}
             <|> do {reserved "nominated"
                    ; return Nominated}
             <|> do {reserved "tied"
                    ; return Tied}
             <|> do {reserved "eliminated"
                    ; return Eliminated}
             <|> do {nm <- identifier
                    ; return $ N nm}
             <|> do {reserved "winner"
                    ; reserved "of"
                    ; cr <- compReference
                    ; return $ Winner cr}
             <|> do {reserved "loser"
                    ; reserved "of"
                    ; cr <- compReference
                    ; return $ Loser cr}
             <|> do {reserved "majority"
                    ; reserved "of"
                    ; vr <- voteReference
                    ; tb <- optionMaybe tiebreaker
                    ; return $ Majority vr tb}
             <|> do {reserved "minority"
                    ; reserved "of"
                    ; vr <- voteReference
                    ; tb <- optionMaybe tiebreaker
                    ; return $ Minority vr tb}
             <|> do {(reserved "highest" <|> reserved "most")
                    ; nm <- identifier
                    ; idl <- parens identifierList
                    ; tb <- optionMaybe tiebreaker
                    ; return $ Most nm idl tb}
             <|> do {(reserved "lowest" <|> reserved "least")
                    ; nm <- identifier
                    ; idl <- parens identifierList
                    ; tb <- optionMaybe tiebreaker
                    ; return $ Least nm idl tb}


-- | Parser for references to a round
roundReference = do
    reserved "round"
    n <- number
    return n

-- | Parser for references to a phase
phaseReference = do
    reserved "phase"
    n <- number
    return n

-- | Parser for references to a relative time
timeReference = do {reserved "before"
                   ; return Before}
                <|> do {reserved "after"
                       ; return After}
                <|> do {reserved "instead"
                       ; reserved "of"
                       ; return During}

-- | Parser for references to a competition
compReference = do
    reserved "competition"
    n <- option 0 number
    return $ CRef n

-- | Parser for references to a vote
voteReference = do
    reserved "vote"
    n <- option 0 number
    return $ VRef n

-- | Parser for references to an allocation
allocateReference = do
    reserved "allocation"
    n <- option 0 number
    return $ ARef n

-- | Parser for references to an action, specifically competition, vote, or allocation
actReference = do {cr <- compReference
                  ; return $ Cmp cr}
               <|> do {vr <- voteReference
                  ; return $ Vt vr}
               <|> do {ar <- allocateReference
                  ; return $ Alloc ar}

-- | Parser for values, which are numbers, counters, or results of actions
value = do {n <- number
           ; return $ Num n}
        <|> do {nm <- identifier
               ; return $ Count nm}
        <|> do {reserved "results"
               ; reserved "of"
               ; ref <- actReference
               ; return $ Result ref} 

-- modifier = do {reserved "just"
--               ; rr <- roundReference
--               ; tr <- timeReference
--               ; pr <- phaseReference
--               ; reserved "insert"
--               ; p <- phase
--               ; return $ Jst rr tr pr p}
--            <|> do {reserved "from"
--                ; rr <- roundReference
--                ; tr <- timeReference
--                ; pr <- phaseReference
--                ; reserved "insert"
--                ; p <- phase
--                ; return $ From rr tr pr p}

tiebreaker = do
    reserved "tiebroken"
    return $ Tiebreak Nothing Everyone


-- * Helper Functions

-- | For adding an attribute to a player
addAttribute :: Attribute -> Player -> Player
addAttribute att (P n atts) = P n (att:atts)

-- | For adding a list of attributes to every player in a PlayerInfo
addAttributesPI :: [Attribute] -> PlayerInfo -> PlayerInfo
addAttributesPI [] pi = pi
addAttributesPI (a:as) (PI ps ts b) = addAttributesPI as (PI (map (addAttribute a) ps) ts b) 