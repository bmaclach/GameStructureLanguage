{-# LANGUAGE FlexibleContexts #-}

{- |
This provides functions for parsing a string written in this language into an AST
-}
module Parser (
    parseGame, teamList, team, playerList, player, attributeList, attribute, 
    affiliation, counter
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

-- * Helper Functions

-- | For adding an attribute to a player
addAttribute :: Attribute -> Player -> Player
addAttribute att (P n atts) = P n (att:atts)

-- | For adding a list of attributes to every player in a PlayerInfo
addAttributesPI :: [Attribute] -> PlayerInfo -> PlayerInfo
addAttributesPI [] pi = pi
addAttributesPI (a:as) (PI ps ts b) = addAttributesPI as (PI (map (addAttribute a) ps) ts b) 