module ParserSpec (spec) where

import Test.Hspec

import Parser
import AST

import Prelude hiding (round)

spec :: Spec
spec = do
  describe "affiliation" $ do
    it "parses an affiliation" $
      parseGame affiliation "affiliation called testing" `shouldBe` Affiliation "testing"
  describe "counter" $ do
    it "parses a score counter" $
      parseGame counter "score called points" `shouldBe` Counter "points" Nothing Nothing Nothing
    it "parses a resource counter" $
      parseGame counter "resource called points" `shouldBe` Counter "points" Nothing Nothing Nothing
    it "parses a counter counter" $
      parseGame counter "counter called points" `shouldBe` Counter "points" Nothing Nothing Nothing
    it "parses a counter with starting value" $
      parseGame counter "score called points starting at 20" `shouldBe` Counter "points" (Just 20) Nothing Nothing
    it "parses a counter with minimum value" $
      parseGame counter "score called points with minimum of 0" `shouldBe` Counter "points" Nothing (Just 0) Nothing
    it "parses a counter with maximum value" $
      parseGame counter "score called points with maximum of 100" `shouldBe` Counter "points" Nothing Nothing (Just 100)
    it "parses a counter with starting, minimum, and maximum value" $
      parseGame counter "score called points starting at 20 with minimum of 0 with maximum of 100" `shouldBe` Counter "points" (Just 20) (Just 0) (Just 100)
  describe "attribute" $ do
    it "parses an affiliation" $
      parseGame attribute "affiliation called testing" `shouldBe` Affiliation "testing"
    it "parses a counter" $
      parseGame attribute "score called points" `shouldBe` Counter "points" Nothing Nothing Nothing
  describe "attributeList" $ do
    it "parses a single attribute" $
      parseGame attributeList "affiliation called testing" `shouldBe` [Affiliation "testing"]
    it "parses multiple attributes" $
      parseGame attributeList "score called points, affiliation called testing" `shouldBe` [Counter "points" Nothing Nothing Nothing, Affiliation "testing"]
  describe "player" $ do
    it "parses a player with no attributes" $
      parseGame player "Brooks" `shouldBe` P "Brooks" []
    it "parses a player with attributes" $
      parseGame player "Brooks with affiliation called test" `shouldBe` P "Brooks" [Affiliation "test"]
  describe "playerList" $ do
    it "parses a single player" $
      parseGame playerList "Brooks" `shouldBe` [P "Brooks" []]
    it "parses multiple players" $
      parseGame playerList "Brooks, Test" `shouldBe` [P "Brooks" [], P "Test" []]
  describe "team" $ do
    it "parses a team" $
      parseGame team "Team Jays: Jose, Kevin" `shouldBe` ([P "Jose" [Affiliation "Jays"], P "Kevin" [Affiliation "Jays"]], ["Jays"])
  describe "teamList" $ do
    it "parses a list of teams" $
      parseGame teamList "Team Jays: Jose, Kevin; Team Yankees: John, Joe" `shouldBe` PI [P "Jose" [Affiliation "Jays"], P "Kevin" [Affiliation "Jays"], P "John" [Affiliation "Yankees"], P "Joe" [Affiliation "Yankees"]] ["Jays", "Yankees"] False
    it "parses a list of players" $
      parseGame teamList "Brooks, Test, Brooks2" `shouldBe` PI [P "Brooks" [], P "Test" [], P "Brooks2" []] [] False
    it "parses a list of players to be randomly divided in teams" $
      parseGame teamList "randomly divide Brooks, Test into Team1, Team2" `shouldBe` PI [P "Brooks" [], P "Test" []] ["Team1", "Team2"] True 
    it "parses a list of players all with attributes" $
      parseGame teamList "Brooks, Brooks2 all with counter called happiness" `shouldBe` PI [P "Brooks" [Counter "happiness" Nothing Nothing Nothing], P "Brooks2" [Counter "happiness" Nothing Nothing Nothing]] [] False
  describe "roundReference" $ do
    it "parses a round reference" $
      parseGame roundReference "round 3" `shouldBe` 3
  describe "phaseReference" $ do
    it "parses a phase reference" $
      parseGame phaseReference "phase 3" `shouldBe` 3
  describe "timeReference" $ do
    it "parses before" $
      parseGame timeReference "before" `shouldBe` Before
    it "parses after" $
      parseGame timeReference "after" `shouldBe` After
    it "parses instead of" $
      parseGame timeReference "instead of" `shouldBe` During
  describe "compReference" $ do
    it "parses a competition reference with no number" $
      parseGame compReference "competition" `shouldBe` CRef 0
    it "parses a competition reference with a number" $
      parseGame compReference "competition 3" `shouldBe` CRef 3
  describe "voteReference" $ do
    it "parses a vote reference with no number" $
      parseGame voteReference "vote" `shouldBe` VRef 0
    it "parses a vote reference with a number" $
      parseGame voteReference "vote 3" `shouldBe` VRef 3
  describe "allocateReference" $ do
    it "parses an allocation reference with no number" $
      parseGame allocateReference "allocation" `shouldBe` ARef 0
    it "parses an allocation reference with a number" $
      parseGame allocateReference "allocation 3" `shouldBe` ARef 3
  describe "actReference" $ do
    it "parses a competition reference" $
      parseGame actReference "competition" `shouldBe` Cmp (CRef 0)
    it "parses a vote reference" $
      parseGame actReference "vote" `shouldBe` Vt (VRef 0)
    it "parses an allocation reference" $
      parseGame actReference "allocation" `shouldBe` Alloc (ARef 0)
  describe "value" $ do
    it "parses a number" $
      parseGame value "-4" `shouldBe` Num (-4)
    it "parses a counter name" $
      parseGame value "points" `shouldBe` Count "points"
    it "parses an action result" $
      parseGame value "results of competition 3" `shouldBe` Result (Cmp (CRef 3))
  describe "identifierP" $ do
    it "parses everyone" $
      parseGame identifierP "everyone" `shouldBe` Everyone
    it "parses chance with no identifier list" $
      parseGame identifierP "chance 3" `shouldBe` Chance 3 (IdList [IdVal Everyone (Num 1)] [])
    it "parses chance with an identifier list" $
      parseGame identifierP "chance 3 (nominated)" `shouldBe` Chance 3 (IdList [IdVal Nominated (Num 1)] [])
    it "parses nominated" $
      parseGame identifierP "nominated" `shouldBe` Nominated
    it "parses tied" $
      parseGame identifierP "tied" `shouldBe` Tied
    it "parses eliminated" $
      parseGame identifierP "eliminated" `shouldBe` Eliminated
    it "parses a name" $
      parseGame identifierP "Brooks" `shouldBe` N "Brooks"
    it "parses a competition winner" $
      parseGame identifierP "winner of competition" `shouldBe` Winner (CRef 0)
    it "parses a competition loser" $
      parseGame identifierP "loser of competition" `shouldBe` Loser (CRef 0)
    it "parses a vote majority" $
      parseGame identifierP "majority of vote" `shouldBe` Majority (VRef 0) Nothing
    it "parses a vote minority" $
      parseGame identifierP "minority of vote" `shouldBe` Minority (VRef 0) Nothing
    it "parses a highest counter reference with no identifier list" $
      parseGame identifierP "highest points" `shouldBe` Most "points" (IdList [IdVal Everyone (Num 1)] []) Nothing
    it "parses a highest counter reference" $
      parseGame identifierP "highest points (nominated)" `shouldBe` Most "points" (IdList [IdVal Nominated (Num 1)] []) Nothing
    it "parses a most counter reference with no identifier list" $
      parseGame identifierP "most points" `shouldBe` Most "points" (IdList [IdVal Everyone (Num 1)] []) Nothing
    it "parses a most counter reference" $
      parseGame identifierP "most points (nominated)" `shouldBe` Most "points" (IdList [IdVal Nominated (Num 1)] []) Nothing
    it "parses a lowest counter reference with no identifier list" $
      parseGame identifierP "lowest points (everyone)" `shouldBe` Least "points" (IdList [IdVal Everyone (Num 1)] []) Nothing
    it "parses a lowest counter reference" $
      parseGame identifierP "lowest points (nominated)" `shouldBe` Least "points" (IdList [IdVal Nominated (Num 1)] []) Nothing
    it "parses a least counter reference with no identifier list" $
      parseGame identifierP "least points (everyone)" `shouldBe` Least "points" (IdList [IdVal Everyone (Num 1)] []) Nothing
    it "parses a least counter reference" $
      parseGame identifierP "least points (nominated)" `shouldBe` Least "points" (IdList [IdVal Nominated (Num 1)] []) Nothing
  describe "identifierVal" $ do
    it "parses an identifier with default value" $
      parseGame identifierVal "everyone" `shouldBe` IdVal Everyone (Num 1)
    it "parses an identifier with a given value" $
      parseGame identifierVal "everyone * 2" `shouldBe` IdVal Everyone (Num 2)
  describe "idList" $ do
    it "parses a single identifier" $
      parseGame idList "everyone" `shouldBe` [Everyone]
    it "parses multiple identifiers" $
      parseGame idList "Brooks, Brooks2" `shouldBe` [N "Brooks", N "Brooks2"]
  describe "idValList" $ do
    it "parses a single identifierVal" $
      parseGame idValList "everyone" `shouldBe` [IdVal Everyone (Num 1)]
    it "parses multiple identifierVal" $
      parseGame idValList "everyone, Brooks" `shouldBe` [IdVal Everyone (Num 1), IdVal (N "Brooks") (Num 1)]
  describe "identifierList" $ do
    it "parses an identifier list without any except list" $
      parseGame identifierList "everyone" `shouldBe` IdList [IdVal Everyone (Num 1)] []
    it "parses an identifier list with an except list" $
      parseGame identifierList "everyone except Brooks" `shouldBe` IdList [IdVal Everyone (Num 1)] [N "Brooks"]
  describe "competitor" $ do
    it "parses team as Team" $
      parseGame competitor "team" `shouldBe` Team
    it "parses for team as Team" $
          parseGame competitor "for team" `shouldBe` Team
    it "parses nothing as Individual" $
      parseGame competitor "" `shouldBe` Individual
  describe "competition" $ do
    it "parses scored competition" $
      parseGame competition "scored competition between everyone" `shouldBe` Scored Individual (IdList [IdVal Everyone (Num 1)] [])
    it "parses placed competition" $
      parseGame competition "competition between everyone" `shouldBe`
        Placed Individual (IdList [IdVal Everyone (Num 1)] []) True True
  describe "selfInclude" $ do
    it "parses including self as True" $
      parseGame selfInclude "including self" `shouldBe` True
    it "parses nothing as False" $
      parseGame selfInclude "" `shouldBe` False
  describe "decision" $ do
    it "parses a vote decision" $
      parseGame decision "vote by everyone between everyone" `shouldBe` Vote (IdList [IdVal Everyone (Num 1)] []) (IdList [IdVal Everyone (Num 1)] []) False
    it "parses a nomination decision" $
      parseGame decision "nomination of 2 by Brooks between everyone" `shouldBe` Nomination 2 (IdList [IdVal (N "Brooks") (Num 1)] []) (IdList [IdVal Everyone (Num 1)] []) False
    it "parses an allocation decision" $
      parseGame decision "allocation of votes by everyone" `shouldBe` Allocation "votes" (IdList [IdVal Everyone (Num 1)] [])
    it "parses a directed vote decision" $
      parseGame decision "directed vote by everyone between everyone" `shouldBe` DirectedVote (IdList [IdVal Everyone (Num 1)] []) (IdList [IdVal Everyone (Num 1)] []) False
    it "parses a uses decision with no otherwise specified" $
      parseGame decision "uses? Brooks then (vote by Brooks between nominated)" `shouldBe` Uses (N "Brooks") [Act (Dec (Vote (IdList [IdVal (N "Brooks") (Num 1)] []) (IdList [IdVal Nominated (Num 1)] []) False))] []
    it "parses a uses decision with an otherwise" $
      parseGame decision "uses? Brooks then (vote by Brooks between nominated) otherwise (elimination of Test)" `shouldBe` Uses (N "Brooks") [Act (Dec (Vote (IdList [IdVal (N "Brooks") (Num 1)] []) (IdList [IdVal Nominated (Num 1)] []) False))] [Prog (AU Elimination (IdList [IdVal (N "Test") (Num 1)] []))]
  describe "action" $ do
    it "parses a competition" $
      parseGame action "competition between everyone" `shouldBe`
        Comp (Placed Individual (IdList [IdVal Everyone (Num 1)] []) True True)
    it "parses a decision" $
      parseGame action "vote by everyone between everyone" `shouldBe` Dec (Vote (IdList [IdVal Everyone (Num 1)] []) (IdList [IdVal Everyone (Num 1)] []) False)
  describe "tiebreaker" $ do
    it "parses a tiebreaker with no action" $
      parseGame tiebreaker "Tiebreaker: rocks chance 1 (everyone)" `shouldBe` Tiebreak "rocks" Nothing (Chance 1 (IdList [IdVal Everyone (Num 1)] [])) 
    it "parses a tiebreaker with an action" $
      parseGame tiebreaker "Tiebreaker: rocks competition between everyone chance 1 (everyone)" `shouldBe` Tiebreak "rocks" (Just (Comp (Placed Individual (IdList [IdVal Everyone (Num 1)] []) True True))) (Chance 1 (IdList [IdVal Everyone (Num 1)] [])) 
  describe "tiebreakerReference" $ do
    it "parses a tiebreaker reference" $
      parseGame tiebreakerReference "tiebroken by rocks" `shouldBe` TieRef "rocks"
  describe "counterUpdate" $ do
    it "parses an increase to a counter" $
      parseGame counterUpdate "increase votes by 3" `shouldBe` Increase "votes" (Num 3)
    it "parses a decrease to a counter" $
      parseGame counterUpdate "decrease votes by 3" `shouldBe` Decrease "votes" (Num 3)
    it "parses a set of a counter to a new value" $
      parseGame counterUpdate "set votes to 0" `shouldBe` Set "votes" (Num 0)
  describe "affiliationUpdate" $ do
    it "parses an elimination" $
      parseGame affiliationUpdate "elimination" `shouldBe` Elimination
    it "parses an affiliation addition" $
      parseGame affiliationUpdate "add test" `shouldBe` Add "test"
    it "parses an affiliation removal" $
      parseGame affiliationUpdate "remove test" `shouldBe` Remove "test"
    it "parses an affiliation change" $
      parseGame affiliationUpdate "change test to pass" `shouldBe` Change "test" "pass"
    it "parses an affiliation swap" $
      parseGame affiliationUpdate "swap Kucha, Ogakor" `shouldBe` Swap ["Kucha", "Ogakor"] [] False
    it "parses a number preserving affiliation swap" $
      parseGame affiliationUpdate "number preserving swap Kucha, Ogakor" `shouldBe` Swap ["Kucha", "Ogakor"] [] True
    it "parses an affiliation swap with affiliation additions" $
      parseGame affiliationUpdate "swap Manu, Kama adding Lesu" `shouldBe` Swap ["Manu", "Kama"] ["Lesu"] False
    it "parses a merge with no merge name" $
      parseGame affiliationUpdate "merge Kucha, Ogakor" `shouldBe` Merge ["Kucha", "Ogakor"] Nothing
    it "parses a merge with a merge name" $
      parseGame affiliationUpdate "merge Kucha, Ogakor to Barramundi" `shouldBe` Merge ["Kucha", "Ogakor"] (Just "Barramundi")
  describe "progression" $ do
    it "parses an affiliation update" $
      parseGame progression "elimination of Brooks" `shouldBe` AU Elimination (IdList [IdVal (N "Brooks") (Num 1)] [])
    it "parses a counter update" $
      parseGame progression "increase votes by 3 for Brooks" `shouldBe` CU (Increase "votes" (Num 3)) (IdList [IdVal (N "Brooks") (Num 1)] [])
  describe "phase" $ do
    it "parses an action" $
      parseGame phase "competition between everyone" `shouldBe`
        Act (Comp (Placed Individual (IdList [IdVal Everyone (Num 1)] []) True True))
    it "parses a progression" $
      parseGame phase "elimination of Brooks" `shouldBe` Prog (AU Elimination (IdList [IdVal (N "Brooks") (Num 1)] []))
  describe "phaseList" $ do
    it "parses a single phase" $
      parseGame phaseList "competition between everyone" `shouldBe`
        [Act (Comp (Placed Individual (IdList [IdVal Everyone (Num 1)] []) True True))]
    it "parses multiple phases" $
      parseGame phaseList "competition between everyone. elimination of Brooks" `shouldBe` [Act (Comp (Placed Individual (IdList [IdVal Everyone (Num 1)] []) True True)), Prog (AU Elimination (IdList [IdVal (N "Brooks") (Num 1)] []))]
  describe "modifier" $ do
    it "parses a modifier for just one round" $
      parseGame modifier "just round 2 before phase 3 insert competition between everyone" `shouldBe` Jst 2 Before 3 (Act (Comp (Placed Individual (IdList [IdVal Everyone (Num 1)] []) True True)))
    it "parses a modifier from one round onwards" $
      parseGame modifier "from round 2 before phase 3 insert competition between everyone" `shouldBe` From 2 Before 3 (Act (Comp (Placed Individual (IdList [IdVal Everyone (Num 1)] []) True True)))
  describe "modifierList" $ do
    it "parses a single modifier" $
      parseGame modifierList "just round 2 before phase 3 insert competition between everyone" `shouldBe` [Jst 2 Before 3 (Act (Comp (Placed Individual (IdList [IdVal Everyone (Num 1)] []) True True)))]
    it "parses multiple modifiers" $
      parseGame modifierList "just round 2 before phase 3 insert competition between everyone. from round 2 before phase 3 insert competition between everyone" `shouldBe` [Jst 2 Before 3 (Act (Comp (Placed Individual (IdList [IdVal Everyone (Num 1)] []) True True))), From 2 Before 3 (Act (Comp (Placed Individual (IdList [IdVal Everyone (Num 1)] []) True True)))]
  describe "round" $ do
    it "parses a round with no repetitions or modifiers" $
      parseGame round "competition between everyone. elimination of Brooks" `shouldBe` R [Act (Comp (Placed Individual (IdList [IdVal Everyone (Num 1)] []) True True)), Prog (AU Elimination (IdList [IdVal (N "Brooks") (Num 1)] []))] 1 []
    it "parses a round with repetitions but no modifiers" $
      parseGame round "competition between everyone. elimination of Brooks repeated 9 times" `shouldBe` R [Act (Comp (Placed Individual (IdList [IdVal Everyone (Num 1)] []) True True)), Prog (AU Elimination (IdList [IdVal (N "Brooks") (Num 1)] []))] 9 []
    it "parses a round with repetitions and modifiers" $
      parseGame round "competition between everyone. elimination of Brooks repeated 9 times with modifications: just round 2 before phase 3 insert competition between everyone. from round 2 before phase 3 insert competition between everyone" `shouldBe` R [Act (Comp (Placed Individual (IdList [IdVal Everyone (Num 1)] []) True True)), Prog (AU Elimination (IdList [IdVal (N "Brooks") (Num 1)] []))] 9 [Jst 2 Before 3 (Act (Comp (Placed Individual (IdList [IdVal Everyone (Num 1)] []) True True))), From 2 Before 3 (Act (Comp (Placed Individual (IdList [IdVal Everyone (Num 1)] []) True True)))]
  describe "roundList" $ do
    it "parses a single round" $ 
        parseGame roundList "competition between everyone. elimination of Brooks;" `shouldBe` [R [Act (Comp (Placed Individual (IdList [IdVal Everyone (Num 1)] []) True True)), Prog (AU Elimination (IdList [IdVal (N "Brooks") (Num 1)] []))] 1 []]
    it "parses multiple rounds" $ 
        parseGame roundList "competition between everyone. elimination of Brooks; elimination of Test;" `shouldBe` [R [Act (Comp (Placed Individual (IdList [IdVal Everyone (Num 1)] []) True True)), Prog (AU Elimination (IdList [IdVal (N "Brooks") (Num 1)] []))] 1 [], R [Prog (AU Elimination (IdList [IdVal (N "Test") (Num 1)] []))] 1 []]
  describe "goal" $ do
    it "parses a goal" $
      parseGame goal "100 votes" `shouldBe` Gl 100 "votes"
  describe "goalList" $ do
    it "parses a single goal" $
      parseGame goalList "100 votes" `shouldBe` [Gl 100 "votes"]
    it "parses multiple goals" $
      parseGame goalList "100 votes, 20 points" `shouldBe` [Gl 100 "votes", Gl 20 "points"]
  describe "winCondition" $ do
    it "parses survive" $
      parseGame winCondition "survive" `shouldBe` Survive
    it "parses jury member vote" $
      parseGame winCondition "7 member jury vote" `shouldBe` Jury 7
    it "parses final competition" $
      parseGame winCondition "competition" `shouldBe` FinalComp Individual
    it "parses counter goal" $
      parseGame winCondition "reach 100 points" `shouldBe` Reach [Gl 100 "points"] Individual
    it "parses identifier list" $
      parseGame winCondition "highest points (everyone)" `shouldBe` Ids (IdList [IdVal (Most "points" (IdList [IdVal Everyone (Num 1)] []) Nothing) (Num 1)] []) Individual
  describe "game" $ do
    it "parses a game with no tiebreakers" $
      parseGame game "Players: Brooks, Test Rounds: competition between everyone. elimination of Brooks; Win: Survive" `shouldBe` G (PI [P "Brooks" [], P "Test" []] [] False) [R [Act (Comp (Placed Individual (IdList [IdVal Everyone (Num 1)] []) True True)), Prog (AU Elimination (IdList [IdVal (N "Brooks") (Num 1)] []))] 1 []] Survive []
    it "parses a game with 1 tiebreaker" $
      parseGame game "Players: Brooks, Test Rounds: competition between everyone. elimination of Brooks; Win: Survive Tiebreaker: rocks chance 1 (everyone)" `shouldBe` G (PI [P "Brooks" [], P "Test" []] [] False) [R [Act (Comp (Placed Individual (IdList [IdVal Everyone (Num 1)] []) True True)), Prog (AU Elimination (IdList [IdVal (N "Brooks") (Num 1)] []))] 1 []] Survive [Tiebreak "rocks" Nothing (Chance 1 (IdList [IdVal Everyone (Num 1)] []))]
    it "parses a game with 2 tiebreakers" $
      parseGame game "Players: Brooks, Test Rounds: competition between everyone. elimination of Brooks; Win: Survive Tiebreaker: rocks chance 1 (everyone) Tiebreaker: Overtime competition between everyone winner of competition" `shouldBe` G (PI [P "Brooks" [], P "Test" []] [] False) [R [Act (Comp (Placed Individual (IdList [IdVal Everyone (Num 1)] []) True True)), Prog (AU Elimination (IdList [IdVal (N "Brooks") (Num 1)] []))] 1 []] Survive [Tiebreak "rocks" Nothing (Chance 1 (IdList [IdVal Everyone (Num 1)] [])), Tiebreak "Overtime" (Just (Comp (Placed Individual (IdList [IdVal Everyone (Num 1)] []) True True))) (Winner (CRef 0))]