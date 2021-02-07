{- |
This defines the AST for the language defining game structures.
-}
module AST (
    Name, Number, Game(..), PlayerInfo(..), Player(..), Attribute(..), 
    Round(..), TimeRef(..), Modifier(..), Phase(..), Action(..), Competitor(..),
    Competition(..), Decision(..), Progression(..), AffiliationUpdate(..),
    CounterUpdate(..), IdentifierList(..), IdentifierVal(..), Identifier(..), 
    Value(..), ActRef(..), CompRef(..), VoteRef(..), AllocRef(..), 
    Tiebreaker(..), TiebreakerRef(..), Goal(..), WinCondition(..)
) where

-- | A name is represented by a string
type Name = String

-- | A number is represented by an integer
type Number = Integer

-- | A game is information about the players, a list of team names, a list of rounds, a list of win conditions, and a potentially empty list of tiebreaker definitions
data Game = G PlayerInfo [Round] WinCondition [Tiebreaker] deriving (Show, Eq)

-- | Player information includes the list of players, the list of teams, and a boolean that, if true, means the players should be randomly and evenly divided between the teams
data PlayerInfo = PI [Player] [Name] Bool deriving (Show, Eq)

-- | A player has a name and a list of attributes
data Player = P Name [Attribute] deriving (Show, Eq)

-- | An attribute is either an affiliation or a counter
data Attribute 
    -- | An affiliation has a name
    = Affiliation Name 
    -- | A counter has a name and potentially 3 numbers: a starting value, a 
    -- minimum value, and a maximum value
    | Counter Name (Maybe Number) (Maybe Number) (Maybe Number)
    deriving (Show, Eq)

-- | A round is a list of phases, a number referring to how many times the round will be played, and a list of modifiers
data Round = R [Phase] Number [Modifier] deriving (Show, Eq)

-- | A reference relative to a given time can be "before", "after", or "during"
data TimeRef = Before | After | During deriving (Show, Eq)

-- | A modifier can effect either "just" one round or "from" a certain round onwards
-- A modifier is parameterized by a number referring to the round to be modified, a TimeRef and another number that together determine which phase should be modified, and a phase representing the modified phase to be inserted in the round.
data Modifier = Jst Number TimeRef Number Phase | From Number TimeRef Number Phase deriving (Show, Eq)

-- | A phase is either an action or a progression.
data Phase = Act Action | Prog Progression deriving (Show, Eq)

-- | An action is either a competition or a decision
data Action = Comp Competition | Dec Decision deriving (Show, Eq)

-- | A competitor is either a team or an individual
data Competitor = Team | Individual deriving (Show, Eq)

-- | A competition is either scored or placed (i.e. first place, second place, etc.)
-- A competition is parameterized by whether it is played between teams or individuals, and by the identifiers for the players who compete. The booleans represent whether it is needed to know the winner (True if yes) and loser (True if yes), respectively.
data Competition = Scored Competitor IdentifierList | Placed Competitor IdentifierList Bool Bool deriving (Show, Eq)

-- | A decision is either a vote, nomination, allocation, directed vote, or a binary decision on whether something is used.
data Decision 
    -- | A vote is parameterized by a list of identifiers who vote, a list of identifiers for who can be voted for, and a Boolean that, if true, means one can vote for themself.
    = Vote IdentifierList IdentifierList Bool 
    -- | A nomination is parameterized by a number of people to be nominated, an identifier for who will do the nominating, a list of identifiers for who is eligible to be nominated, and a Boolean that, if true, means one can nominate themself.
    | Nomination Number IdentifierList IdentifierList Bool
    -- | An allocation is parameterized by a name referring to the resource to be allocated, and a list of identifiers who will do the allocating.
    | Allocation Name IdentifierList
    -- | A directed vote is parameterized by a list of identifiers who vote, a list of identifiers for who can be voted for, and a Boolean that, if true, means one can vote for themself.
    | DirectedVote IdentifierList IdentifierList Bool
    -- | A uses decision is parameterized by an identifier for who will make the decision, a list of phases to occur if the decision is "yes", and a list of phases to occur if the decision is "no"
    | Uses Identifier [Phase] [Phase]
    deriving (Show, Eq)

-- | A progression is either an update to affiliations or to counters, for a given list of identifiers
data Progression = AU AffiliationUpdate IdentifierList | CU CounterUpdate IdentifierList deriving (Show, Eq)

-- | Represents changes to a players lists of affiliations
data AffiliationUpdate 
    -- | For when a player should be removed from the active player list
    = Elimination 
    -- | For when a player should be given a new affiliation
    | Add Name 
    -- | For when a player should have an existing affiliation removed
    | Remove Name 
    -- | For when a player should have an affiliation changed to a different affiliation
    | Change Name Name 
    -- | For when all players with an affiliation in the given list should be randomly assigned an affiliation from the same list, or an affiliation from the second list. If the given Boolean is True, then the number of players with each affiliation should not change, otherwise the numbers should be made even between affiliations.
    | Swap [Name] [Name] Bool 
    -- | For when all players with an affiliation in the given list should have that affiliation removed and replaced with a single affiliation
    | Merge [Name] (Maybe Name)
    deriving (Show, Eq)

-- | Represents changes to a given counter for certain players
data CounterUpdate 
    -- | For when a given counter should be increased by some value
    = Increase Name Value 
    -- | For when a given counter should be decreased by some value
    | Decrease Name Value 
    -- | For when a given counter should be set to some value
    | Set Name Value
    deriving (Show, Eq)

-- | An identifier list is a list of identifiers and a second list of identifiers to not include in the list
data IdentifierList = IdList [IdentifierVal] [Identifier] deriving (Show, Eq)

-- | Groups together an identifier with a value representing the number of times that identifier should be considered
data IdentifierVal = IdVal Identifier Value deriving (Show, Eq)

-- | A representation of a reference to a player or group of players
data Identifier 
    -- | Refers to every active player in the game
    = Everyone 
    -- | Refers to a random player between a pool of potential players
    | Chance Number IdentifierList 
    -- | Refers to players who were nominated as part of a nomination decision
    | Nominated 
    -- | Refers to players who are involved in a tie
    | Tied 
    -- | Refers to players who were once active in the game but have been eliminated
    | Eliminated 
    -- | Refers to a player by their unique name
    | N Name 
    -- | Refers to all players with a given affiliation
    | A Name
    -- | Refers to the winner of a competition
    | Winner CompRef
    -- | Refers to the loser of a competition
    | Loser CompRef 
    -- | Refers to the player who received the majority of the votes, where the tiebreaker determines what should be done in the case of a tie.
    | Majority VoteRef (Maybe TiebreakerRef) 
    -- | Refers to the player who received the minority of the votes, where the tiebreaker determines what should be done in the case of a tie.
    | Minority VoteRef (Maybe TiebreakerRef) 
    -- | Refers to the player who has the most of a given counter, considering only those players referenced by the list of identifiers, with the tiebreaker determining what should be done in the case of a tie.
    | Most Name IdentifierList (Maybe TiebreakerRef)
    -- | Refers to the player who has the least of a given counter, considering only those players referenced by the list of identifiers, with the tiebreaker determining what should be done in the case of a tie. 
    | Least Name IdentifierList (Maybe TiebreakerRef)
    deriving (Show, Eq)

-- | A value is any reference that resolves to a number
data Value 
    -- | For when the value is a literal number
    = Num Number
    -- | For when the value is of a given counter
    | Count Name
    -- | For when the value comes from the results of an action
    | Result ActRef
    deriving (Show, Eq)

-- | For a reference to a previous action, either a competition, vote, or allocation
data ActRef = Cmp CompRef | Vt VoteRef | Alloc AllocRef deriving (Show, Eq)

-- | A competition is referenced by a number for when it occured
data CompRef = CRef Number deriving (Show, Eq)

-- | A vote is referenced by a number for when it occured
data VoteRef = VRef Number deriving (Show, Eq)

-- | An allocation is referenced by a number for when it occured
data AllocRef = ARef Number deriving (Show, Eq)

-- | A tiebreaker is named and might include an action before choosing an identifier to resolve a tie
data Tiebreaker = Tiebreak Name (Maybe Action) Identifier deriving (Show, Eq)

-- | A tiebreaker is referenced by its name
data TiebreakerRef = TieRef Name deriving (Show, Eq)

-- | A goal is a number and the name of a counter for which the number must be reached
data Goal = Gl Number Name deriving (Show, Eq)

-- | A win condition must be satisfied for a player to win the game
data WinCondition
    -- | For when every active player left after all rounds have been completed wins 
    = Survive 
    -- | For when the winner is decided by a jury vote by a number of eliminated players
    | Jury Number 
    -- | For when the winner is either a team or individual, decided by a final competition
    | FinalComp Competitor 
    -- | For when the winner is the first team or individual to reach a certain value for a certain counter
    | Reach [Goal] Competitor 
    -- | For when the winner is either a team or individual in a given list of identifiers
    | Ids IdentifierList Competitor
    deriving (Show, Eq)
