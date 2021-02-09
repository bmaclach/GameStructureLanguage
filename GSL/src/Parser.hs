{-# LANGUAGE FlexibleContexts #-}

{- |
This provides functions for parsing a string written in this language into an AST
-}
module Parser (
    parseGame, teamList, team, playerList, player, attributeList, attribute, 
    affiliation, counter, roundReference, phaseReference, timeReference,
    compReference, voteReference, allocateReference, actReference, value,
    identifierList, idValList, idList, identifierVal, identifierP, competitor,
    competition, selfInclude, decision, action, tiebreaker, 
    tiebreakerReference, counterUpdate, affiliationUpdate, progression, phase, 
    phaseList, modifier, modifierList, round, roundList, goal, goalList, 
    winCondition, game
) where

import Text.Parsec
import Scanner
import AST
import Data.Functor.Identity
import Prelude hiding (round)

-- | This takes a string representation of a game, parses it into an AST, and returns the AST, or an error if one occurs.
parseGame :: (Stream s Identity t) => Parsec s () a -> s -> a
parseGame parser gametoparse = case parse parser "" gametoparse of
    Left e -> error $ show e
    Right ast -> ast

-- | Parser for a complete game description
game = do
    whiteSpace
    reserved "Players"
    colon
    pi <- teamList
    reserved "Rounds"
    colon
    rl <- roundList
    reserved "Win"
    colon
    wc <- winCondition
    tbs <- manyTill tiebreaker eof
    return $ G pi rl wc tbs

-- * Player-related parsers

-- | Parser for a team list, which can be either a list of teams and their associated players, a list of players (if there are no teams), or a list of players and teams and an instruction to randomly divide the players between the teams
teamList = do 
    everyone <- teamTeamList <|> teamPlayerList <|> teamRandomList
    al <- option [] (do {reserved "all"
                       ; reserved "with"
                       ; attributeList
                    })
    return $ addAttributesPI al everyone

teamTeamList = do 
    tl <- sepBy1 team semi
    return $ PI (concatMap fst tl) (concatMap snd tl) False

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
    return (map (addAttribute (Affiliation teamNm)) pl, [teamNm])

-- | Parser for a list of players
playerList = commaSep player

-- | Parser for a player
player = do
    nm <- identifier
    al <- option [] (do {reserved "with"
                        ; attributeList
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
    Affiliation <$> identifier

-- | Parser for counters, returns an Attribute AST node
counter = do
    reserved "score" <|> reserved "resource" <|> reserved "counter"
    reserved "called"
    nm <- identifier
    start <- optionMaybe (do {reserved "starting"
                             ; reserved "at"
                             ; number
                             })
    min <- optionMaybe (try (do {reserved "with"
                           ; reserved "minimum"
                           ; reserved "of"
                           ; number
                           }))
    max <- optionMaybe (do {reserved "with"
                           ; reserved "maximum"
                           ; reserved "of"
                           ; number
                           })
    return $ Counter nm start min max

-- * Round-related parsers

-- | Parses a semicolon-separated list of rounds
roundList = endBy1 round semi

-- | Parses a round, which is a phase list, a number of repetitions, and maybe some modifiers
round = do
    pl <- phaseList
    (n, ml) <- option (1, []) (do {reserved "repeated"
                                  ; times <- number
                                  ; reserved "times"
                                  ; mods <- option [] (do {reserved "with"
                                                      ; reserved "modifications"
                                                      ; colon
                                                      ; modifierList})
                                  ; return (times, mods)})
    return $ R pl n ml

-- | Parses a period-separated list of phases
phaseList = sepBy1 phase dot

-- | Parses a phase, which is either an action or progression
phase = do { Act <$> action }
        <|> do { Prog <$> progression }

-- | Parses an action, which can be a competition or decision
action = do { Comp <$> competition }
         <|> do { Dec <$> decision }

-- | Parses a progression, which is an update of either affiliations or counters, for a given identifier list
progression = do {au <- affiliationUpdate
                 ; reserved "for" <|> reserved "of"
                 ; AU au <$> identifierList }
              <|> do {cu <- counterUpdate
                     ; reserved "for" <|> reserved "of"
                     ; CU cu <$> identifierList }

parseVote = do
  reserved "vote"
  reserved "by"
  voters <- identifierList
  reserved "between"
  votees <- identifierList
  return (voters, votees)

-- | Parses decisions, which can be votes, nominations, allocations, directed votes, or binary uses decisions
decision = do {(voters, votees) <- parseVote
               ; Vote voters votees <$> selfInclude }
           <|> do {reserved "nomination"
                  ; reserved "of"
                  ; n <- number
                  ; reserved "by"
                  ; nominators <- identifierList
                  ; reserved "between"
                  ; pool <- identifierList
                  ; Nomination n nominators pool <$> selfInclude}
           <|> do {reserved "allocation"
                  ; reserved "of"
                  ; nm <- identifier
                  ; reserved "by"
                  ; Allocation nm <$> identifierList}
           <|> do {reserved "directed"
                  ; (voters, votees) <- parseVote
                  ; DirectedVote voters votees <$> selfInclude}
           <|> do {reserved "uses?"
                  ; decider <- identifierP
                  ; reserved "then"
                  ; pl <- parens phaseList
                  ; opl <- option [] (do {reserved "otherwise"
                                         ; parens phaseList})
                  ; return $ Uses decider pl opl}

-- | Parses "including self" as True or "" as False
selfInclude = option False (do {reserved "including"
                               ; reserved "self"
                               ; return True})

parseCompetitor = do
  cmp <- competitor
  reserved "competition"
  reserved "between"
  return cmp

-- | Parser for competitions, which can be scored or not, between teams or individuals, and between certain players
competition = do {reserved "scored"
                 ; cmp <- parseCompetitor
                 ; Scored cmp <$> identifierList}
              <|> do {cmp <- parseCompetitor
                     ; il <- identifierList
                     ; return $ Placed cmp il True True}

-- | Parser for competitors, either team or individual
competitor = option Individual (do {optional (reserved "for")
                                   ; reserved "team"
                                   ; return Team})

-- | Parser for an update to affiliations, which can be eliminating, adding/removing/changing an affiliation, swapping players with certain affiliations, or merging affiliations
affiliationUpdate = do {reserved "elimination"
                       ; return Elimination}
                    <|> do {reserved "add"
                           ; Add <$> identifier}
                    <|> do {reserved "remove"
                           ; Remove <$> identifier}
                    <|> do {reserved "change"
                           ; old <- identifier
                           ; reserved "to"
                           ; Change old <$> identifier}
                    <|> do {np <- option False (do {reserved "number"
                                                   ; reserved "preserving"
                                                   ; return True})
                           ; reserved "swap"
                           ; al <- commaSep identifier
                           ; addition <- option [] (do {reserved "adding"
                                                       ; commaSep identifier})
                           ; return $ Swap al addition np}
                    <|> do {reserved "merge"
                           ; al <- commaSep identifier
                           ; newa <- optionMaybe (do {reserved "to"
                                                     ; identifier})
                           ; return $ Merge al newa}

-- | Parser for an update to a counter, which can be an increase or decrease or setting it to a given value
counterUpdate = do {reserved "increase"
                   ; nm <- identifier
                   ; reserved "by"
                   ; Increase nm <$> value}
                <|> do {reserved "decrease"
                       ; nm <- identifier
                       ; reserved "by"
                       ; Decrease nm <$> value}
                <|> do {reserved "set"
                       ; nm <- identifier
                       ; reserved "to"
                       ; Set nm <$> value}

-- | Parser for identifier lists, which are defined by a list of identifiers to include and a list of identifiers to exclude
identifierList = do
    idv <- idValList
    idl <- option [] (do {reserved "except"
                         ; idList})
    return $ IdList idv idl

-- | Parser for simple comma-separated lists of identifier-value pairs
idValList = commaSep identifierVal

-- | Parser for simple comma-separated lists of identifiers
idList = commaSep identifierP

-- | Parser for identifiers paired with a value. If no value is specified, 1 is used by default.
identifierVal = do
    ident <- identifierP
    val <- option (Num 1) (do {reservedOp "*"
                        ; value})
    return $ IdVal ident val

parseVoteResult = do
  reserved "of"
  vr <- voteReference
  tb <- optionMaybe tiebreakerReference
  return (vr, tb)

parseResourceResult = do
  nm <- identifier
  idl <- option (IdList [IdVal Everyone (Num 1)] []) (parens identifierList)
  tb <- optionMaybe tiebreakerReference
  return (nm, idl, tb)

-- | Parser for identifiers, which can be everyone, chance, nominated, tied, eliminated, a name, the winner or loser of a competition, the majority or minority of a vote, or the player with the highest or lowest value of a counter
identifierP = do {reserved "everyone"
                ; return Everyone}
             <|> do {reserved "chance"
                    ; num <- number
                    ; il <- option (IdList [IdVal Everyone (Num 1)] []) (parens identifierList)
                    ; return $ Chance num il}
             <|> do {reserved "nominated"
                    ; return Nominated}
             <|> do {reserved "tied"
                    ; return Tied}
             <|> do {reserved "eliminated"
                    ; return Eliminated}
             <|> do {N <$> identifier}
             <|> do {reserved "winner"
                    ; reserved "of"
                    ; Winner <$> compReference}
             <|> do {reserved "loser"
                    ; reserved "of"
                    ; Loser <$> compReference}
             <|> do {reserved "majority"
                    ; (vr, tb) <- parseVoteResult
                    ; return $ Majority vr tb}
             <|> do {reserved "minority"
                    ; (vr, tb) <- parseVoteResult
                    ; return $ Minority vr tb}
             <|> do {reserved "highest" <|> reserved "most"
                    ; (nm, idl, tb) <- parseResourceResult
                    ; return $ Most nm idl tb}
             <|> do {reserved "lowest" <|> reserved "least"
                    ; (nm, idl, tb) <- parseResourceResult
                    ; return $ Least nm idl tb}

-- | Parser for references to a round
roundReference = do
    reserved "round"
    number

-- | Parser for references to a phase
phaseReference = do
    reserved "phase"
    number

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
actReference = do {Cmp <$> compReference}
               <|> do {Vt <$> voteReference}
               <|> do {Alloc <$> allocateReference}

-- | Parser for values, which are numbers, counters, or results of actions
value = do {Num <$> number}
        <|> do {Count <$> identifier}
        <|> do {reserved "results"
               ; reserved "of"
               ; Result <$> actReference} 

-- | Parses period-separated lists of modifiers
modifierList = sepBy1 modifier dot

parseModifier = do
  rr <- roundReference
  tr <- timeReference
  pr <- phaseReference
  reserved "insert"
  return (rr, tr, pr)

-- | Parses modifiers, which are instructions to change a given phase in a given round
modifier = do {reserved "just"
              ; (rr, tr, pr) <- parseModifier
              ; Jst rr tr pr <$> phase}
           <|> do {reserved "from"
               ; (rr, tr, pr) <- parseModifier
               ; From rr tr pr <$> phase}

-- | Parses tiebreakers
tiebreaker = do
    reserved "Tiebreaker"
    colon
    nm <- identifier
    act <- optionMaybe action
    Tiebreak nm act <$> identifierP

-- | Parses tiebreaker references
tiebreakerReference = do
    reserved "tiebroken"
    reserved "by"
    TieRef <$> identifier

-- * Win condition-related parsers

-- | Parses win conditions, which are either a goal value for a counter, survival, a jury vote, a final competition, or a list of identifiers. Wins could be either individual or teams.
winCondition = do {reserved "survive"
                  ; return Survive}
               <|> do {n <- number
                      ; reserved "member"
                      ; reserved "jury"
                      ; reserved "vote"
                      ; return $ Jury n}
               <|> do {c <- competitor
                      ; reserved "competition"
                      ; return $ FinalComp c}
               <|> do {reserved "reach"
                      ; gl <- goalList
                      ; Reach gl <$> competitor}
               <|> do {il <- identifierList
                      ; Ids il <$> competitor}

-- | Parses a comma-separated list of goals
goalList = commaSep goal

-- | Parses goals, which are counter names and numbers
goal = do
    n <- number
    Gl n <$> identifier

-- * Helper Functions

-- | For adding an attribute to a player
addAttribute :: Attribute -> Player -> Player
addAttribute att (P n atts) = P n (att:atts)

-- | For adding a list of attributes to every player in a PlayerInfo
addAttributesPI :: [Attribute] -> PlayerInfo -> PlayerInfo
addAttributesPI [] pi = pi
addAttributesPI (a:as) (PI ps ts b) = addAttributesPI as (PI (map (addAttribute a) ps) ts b) 