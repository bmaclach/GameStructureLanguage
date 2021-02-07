{- |
This defines functions for parsing the "tokens" in the language, such as identifiers, keywords, numbers, and symbols.
-}
module Scanner (
    identifier, reserved, reservedOp, number, colon, semi, commaSep, parens, 
    dot, whiteSpace
) where

import Text.Parsec
import Text.Parsec.Char
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (emptyDef)

-- | Sets some options for scanning the language. 
-- Identifiers must start with any letter.
-- The characters following the first letter of an identifier can be letters or numbers.
-- All keywords and operator symbols are reserved and cannot be used as identifiers.
-- The keywords are not case sensitive.
languageDef = emptyDef {
    T.identStart = letter,
    T.identLetter = alphaNum,
    T.reservedNames = ["players", "rounds", "win", "randomly", "divide", "into",
        "all", "with", "team", "affiliation", "score", "resource", "counter", 
        "called", "starting", "at", "minimum", "maximum", "of", "just", "from",
        "before", "after", "instead", "insert", "round", "phase", "repeated",
        "times", "scored", "competition", "between", "including", "self", 
        "vote", "by", "nomination", "allocation", "directed", "uses?", "then", 
        "otherwise", "for", "elimination", "add", "remove", "number", 
        "preserving", "swap", "adding", "change", "to", "merge", "increase", 
        "decrease", "set", "results", "except", "everyone", "chance", 
        "nominated", "tied", "eliminated", "winner", "loser", "majority", 
        "minority", "highest", "lowest", "most", "least", "tiebroken", "tiebreaker", "member", "jury", "survive", "reach"],
    T.reservedOpNames = ["*"],
    T.caseSensitive = False
}

-- | A parser for tokens based on the language definition.
scanner = T.makeTokenParser languageDef

-- | Parses identifiers
identifier = T.identifier scanner

-- | Parses keywords
reserved = T.reserved scanner

-- | Parses operators
reservedOp = T.reserved scanner

-- | Parses integers, including the sign and number.
number = T.integer scanner

-- | Parses a colon \':\'
colon = T.colon scanner

-- | Parses a semi-colon ';'
semi = T.semi scanner

-- | Parses a comma-separated list of items, which must include at least one item.
commaSep = T.commaSep1 scanner

-- | Parses opening and closing parentheses, and returns the result of parsing in between the parentheses.
parens = T.parens scanner

-- | Parses a period '.'
dot = T.dot scanner

-- | Parses one or more spaces.
whiteSpace = T.whiteSpace scanner