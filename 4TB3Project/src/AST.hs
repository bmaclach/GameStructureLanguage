{- |
This defines the AST for the language defining game structures.
-}
module AST (
    Name, Number, Game(..), Player(..), Attribute(..), Round(..), TimeRef(..), 
    Modifier(..)
) where

type Name = String

type Number = Integer

-- | A game is a list of players, a list of rounds, a list of modifiers, and a list of win conditions
data Game = G [Player] [Round] [Modifier] [WinCondition]

-- | A player has a name and a list of attributes
data Player = P Name [Attribute]

-- | An attribute is either an affiliation or a counter
data Attribute 
    -- | An affiliation has a name
    = Affiliation Name 
    -- | A counter has a name and potentially 3 numbers: a starting value, a 
    -- minimum value, and a maximum value
    | Counter Name (Maybe Number) (Maybe Number) (Maybe Number)

-- | A round is a list of phases and a number referring to how many times the round will be played
data Round = R [Phase] Number [Modifier]

-- | A reference relative to a given time can be "before", "after", or "during"
data TimeRef = Before | After | During

-- | A modifier can effect either "just" one round or "from" a certain round onwards
-- A modifier is parameterized by a number referring to the round to be modified, a TimeRef and another number that together determine which phase should be modified, and a phase representing the modified phase to be inserted in the round.
data Modifier = Just Number TimeRef Number Phase | From Number TimeRef Number Phase



