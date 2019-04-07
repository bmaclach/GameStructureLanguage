import Test.Hspec

import Parser
import AST
import PreCompiler
import Compiler

import Prelude hiding (round)
import Text.PrettyPrint.HughesPJ (empty, nest, text)
import Control.Monad.Reader (runReader)

ids :: IdNames
ids = IdNames ["Brooks", "Mac"] ["Test", "Great"] ["votes", "points"]

exPhase1, exPhase2, exPhase3, exPhase4 :: Phase
exPhase1 = Act (Comp (Scored Team (IdList [IdVal Everyone (Num 1)] [])))
exPhase2 = Prog (CU (Increase "votes" (Num 3)) (IdList [IdVal (N "Brooks") (Num 1)] []))
exPhase3 = Prog (AU Elimination (IdList [IdVal (N "Test") (Num 1)] []))
exPhase4 = Act (Dec (Allocation "votes" (IdList [IdVal Everyone (Num 1)] [])))

exGame :: Game
exGame = G (PI [P "Brooks" [], P "Test" []] [] False) [R [exPhase1, exPhase2] 10 [From 8 Before 1 exPhase3], R [exPhase4, exPhase1] 7 [Jst 4 During 2 exPhase3, From 5 After 1 exPhase2]] Survive

main :: IO ()
main = hspec $ do
    describe "Parser" $ do
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
                parseGame identifierP "chance" `shouldBe` Chance (IdList [IdVal Everyone (Num 1)] [])
            it "parses chance with an identifier list" $
                parseGame identifierP "chance (nominated)" `shouldBe` Chance (IdList [IdVal Nominated (Num 1)] [])
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
                parseGame decision "uses? Brooks then (vote by Brooks between nominated)" `shouldBe` Uses (N "Brooks") [Act (Dec (Vote (IdList [IdVal (N "Brooks") (Num 1)] []) (IdList [IdVal (Nominated) (Num 1)] []) False))] []
            it "parses a uses decision with an otherwise" $
                parseGame decision "uses? Brooks then (vote by Brooks between nominated) otherwise (elimination of Test)" `shouldBe` Uses (N "Brooks") [Act (Dec (Vote (IdList [IdVal (N "Brooks") (Num 1)] []) (IdList [IdVal (Nominated) (Num 1)] []) False))] [Prog (AU (Elimination) (IdList [IdVal (N "Test") (Num 1)] []))]
        describe "action" $ do
            it "parses a competition" $
                parseGame action "competition between everyone" `shouldBe`
                Comp (Placed Individual (IdList [IdVal Everyone (Num 1)] []) True True)
            it "parses a decision" $
                parseGame action "vote by everyone between everyone" `shouldBe` Dec (Vote (IdList [IdVal Everyone (Num 1)] []) (IdList [IdVal Everyone (Num 1)] []) False)
        describe "tiebreaker" $ do
            it "parses a tiebreaker with no action" $
                parseGame tiebreaker "tiebroken by rocks: chance (everyone)" `shouldBe` Tiebreak "rocks" Nothing (Chance (IdList [IdVal Everyone (Num 1)] [])) 
            it "parses a tiebreaker with an action" $
                parseGame tiebreaker "tiebroken by rocks: competition between everyone chance (everyone)" `shouldBe` Tiebreak "rocks" (Just (Comp (Placed Individual (IdList [IdVal Everyone (Num 1)] []) True True))) (Chance (IdList [IdVal Everyone (Num 1)] [])) 
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
            it "parses a game" $
                parseGame game "Players: Brooks, Test Rounds: competition between everyone. elimination of Brooks; Win: Survive" `shouldBe` G (PI [P "Brooks" [], P "Test" []] [] False) [R [Act (Comp (Placed Individual (IdList [IdVal Everyone (Num 1)] []) True True)), Prog (AU Elimination (IdList [IdVal (N "Brooks") (Num 1)] []))] 1 []] Survive
    describe "PreCompiler" $ do
        describe "remove0Rounds" $ do
            it "removes rounds with 0 repetitions" $
                remove0Rounds [R [] 1 [], R [] 0 [], R [] 2 []] `shouldBe` [R [] 1 [], R [] 2 []]
        describe "addPhaseToPhaseList" $ do 
            it "inserts a phase before" $
                addPhaseToPhaseList [exPhase1, exPhase2, exPhase3] exPhase4 Before 2 `shouldBe` [exPhase1, exPhase4, exPhase2, exPhase3] 
            it "inserts a phase after" $
                addPhaseToPhaseList [exPhase1, exPhase2, exPhase3] exPhase4 After 2 `shouldBe` [exPhase1, exPhase2, exPhase4, exPhase3]
            it "inserts a phase instead of" $
                addPhaseToPhaseList [exPhase1, exPhase2, exPhase3] exPhase4 During 2 `shouldBe` [exPhase1, exPhase4, exPhase3]
        describe "addPhaseToRound" $ do
            it "adds a phase to a round and removes a modifier" $
                addPhaseToRound exPhase4 During 2 (R [exPhase1, exPhase2, exPhase3] 1 [Jst 3 During 2 exPhase4, From 5 During 1 exPhase4]) `shouldBe` (R [exPhase1, exPhase4, exPhase3] 1 [From 5 During 1 exPhase4])
        describe "applyModifier" $ do
            it "applies a 'just' modifier to a round" $
                applyModifier (R [exPhase1, exPhase2, exPhase3] 5 [Jst 8 Before 2 exPhase4]) 5 `shouldBe` [R [exPhase1, exPhase2, exPhase3] 2 [], R [exPhase1, exPhase4, exPhase2, exPhase3] 1 [], R [exPhase1, exPhase2, exPhase3] 2 []]
            it "applies a 'from' modifier to a round" $
                applyModifier (R [exPhase1, exPhase2, exPhase3] 5 [From 8 Before 2 exPhase4]) 5 `shouldBe` [R [exPhase1, exPhase2, exPhase3] 2 [], R [exPhase1, exPhase4, exPhase2, exPhase3] 3 []]
        describe "applyRoundModifiers" $ do
            it "applies a 'just' modifier to a list of rounds" $
                applyRoundModifiers 0 [R [exPhase1, exPhase2, exPhase3] 2 [Jst 3 Before 2 exPhase4], R [exPhase1, exPhase4, exPhase2, exPhase3] 3 [Jst 3 Before 2 exPhase4]] `shouldBe` [R [exPhase1, exPhase2, exPhase3] 2 [], R [exPhase1, exPhase4, exPhase2, exPhase3] 0 [], R [exPhase1, exPhase4, exPhase4, exPhase2, exPhase3] 1 [], R [exPhase1, exPhase4, exPhase2, exPhase3] 2 []]
            it "applies a 'from' modifier to a list of rounds" $
                applyRoundModifiers 0 [R [exPhase1, exPhase2, exPhase3] 2 [From 4 During 1 exPhase4], R [exPhase1, exPhase4, exPhase2, exPhase3] 3 [From 4 During 1 exPhase4], R [exPhase1, exPhase2, exPhase3] 2 [From 4 During 1 exPhase4]] `shouldBe` [R [exPhase1, exPhase2, exPhase3] 2 [], R [exPhase1, exPhase4, exPhase2, exPhase3] 1 [], R [exPhase4, exPhase4, exPhase2, exPhase3] 2 [], R [exPhase4, exPhase2, exPhase3] 2 []]
        describe "applyModifiers" $ do
            it "applies all modifiers to all rounds in a game" $
                applyModifiers exGame `shouldBe` G (PI [P "Brooks" [], P "Test" []] [] False) [R [exPhase1, exPhase2] 7 [], R [exPhase3, exPhase1, exPhase2] 3 [], R [exPhase4, exPhase1] 3 [], R [exPhase4, exPhase3] 1 [], R [exPhase4, exPhase2, exPhase1] 3 []] Survive
        describe "updateCompInfo" $ do
            it "appends to CompInfo if number not present" $
                updateCompInfo [(1, False, False), (2, True, False)] (3, True, True) `shouldBe` [(1, False, False), (2, True, False), (3, True,True)]
            it "updates existing info if number is present" $
                updateCompInfo [(1, True, False), (2, True, False)] (1, False, True) `shouldBe` [(1, True, True), (2, True, False)]
        describe "combineCompInfo" $ do
            it "properly combines two CompInfos" $
                combineCompInfo [(1, True, False), (2, True, False)] [(3, True, True), (1, False, True)] `shouldBe` [(1, True, True), (2, True, False), (3, True,True)]
            it "returns first list if second is empty" $
                combineCompInfo [(1, True, False), (2, True, False)] [] `shouldBe` [(1, True, False), (2, True, False)]
            it "returns second list if first is empty" $
                combineCompInfo [] [(1, True, False), (2, True, False)] `shouldBe` [(1, True, False), (2, True, False)]
        describe "getCompRefId" $ do
            it "adds to CompInfo if winner encountered" $
                getCompRefId (Winner (CRef 2)) 3 False `shouldBe` [(2, True, False)]
            it "adds to CompInfo if winner encountered with 0 index" $
                getCompRefId (Winner (CRef 0)) 3 False `shouldBe` [(3, True, False)]
            it "adds to CompInfo if loser encountered" $
                getCompRefId (Loser (CRef 2)) 3 False `shouldBe` [(2, False, True)]
            it "adds to CompInfo if loser encountered with 0 index" $
                getCompRefId (Loser (CRef 0)) 3 False `shouldBe` [(3, False, True)]
            it "adds to CompInfo if Chance encountered" $
                getCompRefId (Chance (IdList [IdVal (Winner (CRef 2)) (Num 1)] [])) 3 False 
                    `shouldBe` [(2, True, False)]
            it "adds to CompInfo if Majority encountered with tiebreaker" $
                getCompRefId (Majority (VRef 1) (Just (Tiebreak "a" Nothing (Winner (CRef 2))))) 3 False `shouldBe` [(2, True, False)]
            it "does not add to CompInfo if Majority with tiebreaker with index 0 is encountered" $
                getCompRefId (Majority (VRef 1) (Just (Tiebreak "a" Nothing (Winner (CRef 0))))) 3 False `shouldBe` []
            it "adds to CompInfo if Minority encountered with tiebreaker" $
                getCompRefId (Minority (VRef 1) (Just (Tiebreak "a" Nothing (Winner (CRef 2))))) 3 False `shouldBe` [(2, True, False)]
            it "does not add to CompInfo if Minority with tiebreaker with index 0 is encountered" $
                getCompRefId (Minority (VRef 1) (Just (Tiebreak "a" Nothing (Winner (CRef 0))))) 3 False `shouldBe` []
            it "adds to CompInfo if Most with no tiebreaker encountered" $
                getCompRefId (Most "votes" (IdList [IdVal (Winner (CRef 2)) (Num 1)] []) Nothing) 3 False `shouldBe` [(2, True, False)]
            it "adds to CompInfo if Most with tiebreaker encountered" $
                getCompRefId (Most "votes" (IdList [IdVal (Winner (CRef 2)) (Num 1)] []) (Just (Tiebreak "a" Nothing (Winner (CRef 1))))) 3 False `shouldBe` [(2, True, False), (1, True, False)]
            it "does not add tiebreaker to CompInfo if Most with tiebreaker with index 0 is encountered" $
                getCompRefId (Most "votes" (IdList [IdVal (Winner (CRef 2)) (Num 1)] []) (Just (Tiebreak "a" Nothing (Winner (CRef 0))))) 3 False `shouldBe` [(2, True, False)]
            it "adds to CompInfo if Least with no tiebreaker encountered" $
                getCompRefId (Least "votes" (IdList [IdVal (Winner (CRef 2)) (Num 1)] []) Nothing) 3 False `shouldBe` [(2, True, False)]
            it "adds to CompInfo if Least with tiebreaker encountered" $
                getCompRefId (Least "votes" (IdList [IdVal (Winner (CRef 2)) (Num 1)] []) (Just (Tiebreak "a" Nothing (Winner (CRef 1))))) 3 False `shouldBe` [(2, True, False), (1, True, False)]
            it "does not add tiebreaker to CompInfo if Least with tiebreaker with index 0 is encountered" $
                getCompRefId (Least "votes" (IdList [IdVal (Winner (CRef 2)) (Num 1)] []) (Just (Tiebreak "a" Nothing (Winner (CRef 0))))) 3 False `shouldBe` [(2, True, False)]
        describe "getCompRefIdentifiers" $
            it "adds info from list of identifiers to CompInfo" $
                getCompRefIdentifiers [Winner (CRef 2), Everyone, Loser (CRef 2), N "Brooks"] 3 False `shouldBe` [(2, True, True)]
        describe "getCompRefIdVals" $
            it "adds info from list of IdentifierVals to CompInfo" $
                getCompRefIdVals [IdVal (Winner (CRef 2)) (Num 1), IdVal Everyone (Num 1), IdVal (Loser (CRef 2)) (Num 1), IdVal (N "Brooks") (Num 1)] 3 False `shouldBe` [(2, True, True)]
        describe "getCompRefIdList" $
            it "adds info from IdentifierList to CompInfo" $
                getCompRefIdList (IdList [IdVal (Winner (CRef 2)) (Num 1), IdVal Everyone (Num 1), IdVal (Loser (CRef 2)) (Num 1), IdVal (N "Brooks") (Num 1)] [Winner (CRef 1), Everyone, Loser (CRef 1), N "Brooks"]) 3 False `shouldBe` [(2, True, True), (1, True, True)]
        describe "getCompRefs" $ do
            it "extracts CompInfo from scored comp" $
                getCompRefs [Act (Comp (Scored Team (IdList [IdVal Everyone (Num 1)] [Winner (CRef 1)])))] 2 `shouldBe` [(1, True, False)]
            it "extracts CompInfo from placed comp" $    
                getCompRefs [Act (Comp (Placed Team (IdList [IdVal Everyone (Num 1)] [Winner (CRef 1)]) True True))] 2 `shouldBe` [(1, True, False)]
            it "extracts CompInfo from Vote" $
                getCompRefs [Act (Dec (Vote (IdList [IdVal Everyone (Num 1)] [Loser (CRef 1)]) (IdList [IdVal Everyone (Num 1)] [Winner (CRef 1)]) False))] 2 `shouldBe` [(1, True, True)]
            it "extracts CompInfo from Nomination" $
                getCompRefs [Act (Dec (Nomination 2 (IdList [IdVal Everyone (Num 1)] [Loser (CRef 1)]) (IdList [IdVal Everyone (Num 1)] [Winner (CRef 1)]) False))] 2 `shouldBe` [(1, True, True)]
            it "extracts CompInfo from Allocation" $
                getCompRefs [Act (Dec (Allocation "votes" (IdList [IdVal Everyone (Num 1)] [Loser (CRef 1)])))] 2 `shouldBe` [(1, False, True)]
            it "extracts CompInfo from DirectedVote" $
                getCompRefs [Act (Dec (DirectedVote (IdList [IdVal Everyone (Num 1)] [Loser (CRef 1)]) (IdList [IdVal Everyone (Num 1)] [Winner (CRef 1)]) False))] 2 `shouldBe` [(1, True, True)]
            it "extracts CompInfo from Uses" $
                getCompRefs [Act (Dec (Uses (Winner (CRef 1)) [] []))] 2 `shouldBe` [(1, True, False)]
            it "extracts CompInfo from AffiliationUpdate" $
                getCompRefs [Prog (AU Elimination (IdList [IdVal Everyone (Num 1)] [Loser (CRef 1)]))] 2 `shouldBe` [(1, False, True)]
            it "extracts CompInfo from CounterUpdate" $
                getCompRefs [Prog (CU (Increase "votes" (Num 3)) (IdList [IdVal Everyone (Num 1)] [Loser (CRef 1)]))] 2 `shouldBe` [(1, False, True)]
            it "increases competition counter when scored and placed competitions are encountered" $
                getCompRefs [Act (Comp (Scored Team (IdList [IdVal Everyone (Num 1)] [Winner (CRef 1)]))), Act (Comp (Placed Team (IdList [IdVal Everyone (Num 1)] [Winner (CRef 0)]) True True)), Act (Dec (Allocation "votes" (IdList [IdVal Everyone (Num 1)] [Loser (CRef 0)])))] 1 `shouldBe` [(1, True, False), (2, True, False), (3, False, True)]
        describe "updateComp" $ do
            it "does not change Scored competition" $
                updateComp (Scored Team (IdList [IdVal Everyone (Num 1)] [])) 1 [(1, True, True)] `shouldBe` (Scored Team (IdList [IdVal Everyone (Num 1)] []))
            it "updates Placed competition with info from CompInfo" $
                updateComp (Placed Team (IdList [IdVal Everyone (Num 1)] []) True True) 1 [(2, False, False), (1, False, True)] `shouldBe` (Placed Team (IdList [IdVal Everyone (Num 1)] []) False True)
            it "updates Placed competition with False if it is not in CompInfo" $
                updateComp (Placed Team (IdList [IdVal Everyone (Num 1)] []) True True) 3 [(2, False, False), (1, False, True)] `shouldBe` (Placed Team (IdList [IdVal Everyone (Num 1)] []) False False)
        describe "updatePhaseComps" $ do
            it "updates all Placed competitions in a list of phases" $
                updatePhaseComps [Act (Comp (Placed Team (IdList [IdVal Everyone (Num 1)] []) True True)), Act (Comp (Scored Team (IdList [IdVal Everyone (Num 1)] []))), Act (Comp (Placed Team (IdList [IdVal Everyone (Num 1)] []) True True))] 1 [(1, False, True)] `shouldBe` [Act (Comp (Placed Team (IdList [IdVal Everyone (Num 1)] []) False True)), Act (Comp (Scored Team (IdList [IdVal Everyone (Num 1)] []))), Act (Comp (Placed Team (IdList [IdVal Everyone (Num 1)] []) False False))]
        describe "updateRoundComps" $ do
            it "updates all Placed competitions in a round" $
                updateRoundComps (R [Act (Comp (Placed Team (IdList [IdVal Everyone (Num 1)] []) True True)), Act (Comp (Scored Team (IdList [IdVal (Winner (CRef 0)) (Num 1)] []))), Act (Comp (Placed Team (IdList [IdVal (Loser (CRef 0)) (Num 1)] []) True True))] 5 []) `shouldBe` R [Act (Comp (Placed Team (IdList [IdVal Everyone (Num 1)] []) True False)), Act (Comp (Scored Team (IdList [IdVal (Winner (CRef 0)) (Num 1)] []))), Act (Comp (Placed Team (IdList [IdVal (Loser (CRef 0)) (Num 1)] []) False False))] 5 []
        describe "updateComps" $ do
            it "updates all Placed competitions in a Game" $
                updateComps (G (PI [] [] False) [R [Act (Comp (Placed Team (IdList [IdVal Everyone (Num 1)] []) True True)), Act (Comp (Scored Team (IdList [IdVal (Winner (CRef 0)) (Num 1)] []))), Act (Comp (Placed Team (IdList [IdVal (Loser (CRef 0)) (Num 1)] []) True True))] 5 [], R [Act (Comp (Placed Team (IdList [IdVal Everyone (Num 1)] []) True True)), Act (Comp (Scored Team (IdList [IdVal (Winner (CRef 1)) (Num 1)] []))), Act (Comp (Placed Team (IdList [IdVal (Loser (CRef 1)) (Num 1)] []) True True))] 3 []] Survive) `shouldBe` G (PI [] [] False) [R [Act (Comp (Placed Team (IdList [IdVal Everyone (Num 1)] []) True False)), Act (Comp (Scored Team (IdList [IdVal (Winner (CRef 0)) (Num 1)] []))), Act (Comp (Placed Team (IdList [IdVal (Loser (CRef 0)) (Num 1)] []) False False))] 5 [], R [Act (Comp (Placed Team (IdList [IdVal Everyone (Num 1)] []) True True)), Act (Comp (Scored Team (IdList [IdVal (Winner (CRef 1)) (Num 1)] []))), Act (Comp (Placed Team (IdList [IdVal (Loser (CRef 1)) (Num 1)] []) False False))] 3 []] Survive
        describe "isNameTaken" $ do
            it "returns true if Name is taken" $
                isNameTaken ["Brooks"] "Brooks" `shouldBe` True
            it "returns false if Name is not taken" $
                isNameTaken ["Test"] "Brooks" `shouldBe` False
        describe "getNamesFromPlayerList" $ do
            it "returns names from player list" $
                getNamesFromPlayerList [P "Brooks" [], P "Test" []] `shouldBe` ["Brooks", "Test"]
        describe "getAllNames" $ do
            it "returns all player names in a game" $
                getAllNames (G (PI [P "Brooks" [], P "Test" []] [] False) [] Survive) `shouldBe` ["Brooks", "Test"]
        describe "getAffsFromPhaseList" $ do
            it "returns affiliations introduced in an Add affiliation update" $
                getAffsFromPhaseList [Prog (AU (Add "Jays") (IdList [] []))] [] `shouldBe` ["Jays"]
            it "returns affiliations introduced in a Change affiliation update" $
                getAffsFromPhaseList [Prog (AU (Change "HOH" "houseguest") (IdList [] []))] [] `shouldBe` ["houseguest"]
            it "returns affiliations introduced in a Swap affiliation update" $
                getAffsFromPhaseList [Prog (AU (Swap [] ["Kucha", "Ogakor"] False) (IdList [] []))] [] `shouldBe` ["Kucha", "Ogakor"]
            it "returns affiliations introduced in a Merge affiliation update" $
                getAffsFromPhaseList [Prog (AU (Merge [] (Just "Barramundi")) (IdList [] []))] [] `shouldBe` ["Barramundi"]
            it "returns affiliations introduced in a Merge with no new affiliation specified" $
                getAffsFromPhaseList [Prog (AU (Merge [] Nothing) (IdList [] []))] [] `shouldBe` ["merged"]
            it "returns affiliations from the 'then' branch of a Uses" $
                getAffsFromPhaseList [Act (Dec (Uses (N "Brooks") [Prog (AU (Add "Jays") (IdList [] []))] []))] [] `shouldBe` ["Jays"]
            it "returns affiliations from the 'otherwise' branch of a Uses" $
                getAffsFromPhaseList [Act (Dec (Uses (N "Brooks") [] [Prog (AU (Add "Jays") (IdList [] []))]))] [] `shouldBe` ["Jays"]
            it "returns affiliations from phase lists with multiple entries" $
                getAffsFromPhaseList [Prog (AU (Add "Jays") (IdList [] [])), Prog (AU (Change "HOH" "houseguest") (IdList [] []))] [] `shouldBe` ["Jays", "houseguest"]
        describe "getAffsFromAttList" $ do
            it "does not get names from counters" $
                getAffsFromAttList [Counter "votes" Nothing Nothing Nothing] [] `shouldBe` []
            it "returns name of affiliation" $
                getAffsFromAttList [Affiliation "Jays"] [] `shouldBe` ["Jays"]
            it "returns names of multiple affiliations" $
                getAffsFromAttList [Affiliation "Jays", Counter "votes" Nothing Nothing Nothing, Affiliation "Yankees"] [] `shouldBe` ["Jays", "Yankees"]
        describe "getAffsFromPlayerList" $ do
            it "returns names of a player's affiliations" $
                getAffsFromPlayerList [P "Brooks" [Affiliation "Great"]] [] `shouldBe` ["Great"]
            it "returns names of multiple player's affiliations" $
                getAffsFromPlayerList [P "Brooks" [Affiliation "Great"], P "Test" [Affiliation "Boring"]] [] `shouldBe` ["Great", "Boring"]
        describe "getAllAffiliations" $ do
            it "returns all affiliations in a game plus 'nominee'" $
                getAllAffiliations (G (PI [P "Brooks" [Affiliation "Jays"], P "Test" [Affiliation "Yankees"]] ["Jays", "Yankees"] False) [R [Prog (AU (Add "Kucha") (IdList [] [])), Prog (AU (Add "Jays") (IdList [] []))] 5 []] Survive) [] `shouldBe` ["nominee", "Jays", "Yankees", "Kucha"]
        describe "updateTiebreakIds" $ do
            it "changes N to A in tiebreaker with no action" $
                updateTiebreakIds [] ["Test"] (Tiebreak "a" Nothing (N "Test")) `shouldBe` (Tiebreak "a" Nothing (A "Test"))
            it "changes N to A in tiebreaker with action" $
                updateTiebreakIds [] ["Test"] (Tiebreak "a" (Just (Comp (Scored Team (IdList [] [N "Test"])))) (N "Test")) `shouldBe` (Tiebreak "a" (Just (Comp (Scored Team (IdList [] [A "Test"])))) (A "Test"))
        describe "updateId" $ do
            it "changes an N to an A if it is an affiliation" $
                updateId [] ["Test"] (N "Test") `shouldBe` (A "Test")
            it "does not change an N if it is a player name" $
                updateId ["Test"] [] (N "Test") `shouldBe` (N "Test")
            it "changes an N in a Chance identifier" $
                updateId [] ["Test"] (Chance (IdList [] [N "Test"])) `shouldBe`(Chance (IdList [] [A "Test"]))
            it "changes an N in a Majority tiebreaker" $
                updateId [] ["Test"] (Majority (VRef 1) (Just (Tiebreak "a" Nothing (N "Test")))) `shouldBe` (Majority (VRef 1) (Just (Tiebreak "a" Nothing (A "Test"))))
            it "changes an N in a Minority tiebreaker" $
                updateId [] ["Test"] (Minority (VRef 1) (Just (Tiebreak "a" Nothing (N "Test")))) `shouldBe` (Minority (VRef 1) (Just (Tiebreak "a" Nothing (A "Test"))))
            it "changes an N in a Most with no tiebreaker" $
                updateId [] ["Test"] (Most "votes" (IdList [] [N "Test"]) Nothing) `shouldBe` (Most "votes" (IdList [] [A "Test"]) Nothing)
            it "changes an N in a Most with a tiebreaker" $
                updateId [] ["Test"] (Most "votes" (IdList [] [N "Test"]) (Just (Tiebreak "a" Nothing (N "Test")))) `shouldBe` (Most "votes" (IdList [] [A "Test"]) (Just (Tiebreak "a" Nothing (A "Test"))))
            it "changes an N in a Most with no tiebreaker" $
                updateId [] ["Test"] (Least "votes" (IdList [] [N "Test"]) Nothing) `shouldBe` (Least "votes" (IdList [] [A "Test"]) Nothing)
            it "changes an N in a Most with a tiebreaker" $
                updateId [] ["Test"] (Least "votes" (IdList [] [N "Test"]) (Just (Tiebreak "a" Nothing (N "Test")))) `shouldBe` (Least "votes" (IdList [] [A "Test"]) (Just (Tiebreak "a" Nothing (A "Test"))))
            it "does not change other identifiers" $
                updateId [] ["Test"] Everyone `shouldBe` Everyone
        describe "updateIdentifierIds" $ do
            it "changes N to A in a list of identifiers" $
                updateIdentifierIds ["Brooks"] ["Test"] [N "Brooks", Everyone, N "Test"] `shouldBe` [N "Brooks", Everyone, A "Test"]
        describe "updateIdValIds" $ do
            it "changes N to A in a list of IdentifierVals" $
                updateIdValIds ["Brooks"] ["Test"] [IdVal (N "Brooks") (Num 1), IdVal Everyone (Num 1), IdVal (N "Test") (Num 1)] `shouldBe` [IdVal (N "Brooks") (Num 1), IdVal Everyone (Num 1), IdVal (A "Test") (Num 1)]
        describe "updateIdListIds" $ do
            it "changes N to A in an IdentifierList" $
                updateIdListIds ["Brooks"] ["Test"] (IdList [IdVal (N "Test") (Num 1), IdVal (N "Brooks") (Num 1)] [N "Brooks", N "Test"]) `shouldBe` (IdList [IdVal (A "Test") (Num 1), IdVal (N "Brooks") (Num 1)] [N "Brooks", A "Test"])
        describe "updateActionIds" $ do
            it "changes N to A in a Scored competition" $
                updateActionIds [] ["Test"] (Comp (Scored Team (IdList [] [N "Test"]))) `shouldBe` (Comp (Scored Team (IdList [] [A "Test"])))
            it "changes N to A in a Placed competition" $
                updateActionIds [] ["Test"] (Comp (Placed Team (IdList [] [N "Test"]) False False)) `shouldBe` (Comp (Placed Team (IdList [] [A "Test"]) False False))
            it "changes N to A in a Vote" $
                updateActionIds [] ["Test"] (Dec (Vote (IdList [] [N "Test"]) (IdList [] [N "Test"]) False)) `shouldBe` (Dec (Vote (IdList [] [A "Test"]) (IdList [] [A "Test"]) False))
            it "changes N to A in a Nomination" $
                updateActionIds [] ["Test"] (Dec (Nomination 2 (IdList [] [N "Test"]) (IdList [] [N "Test"]) False)) `shouldBe` (Dec (Nomination 2 (IdList [] [A "Test"]) (IdList [] [A "Test"]) False))
            it "changes N to A in an Allocation" $
                updateActionIds [] ["Test"] (Dec (Allocation "votes" (IdList [] [N "Test"]))) `shouldBe` (Dec (Allocation "votes" (IdList [] [A "Test"])))
            it "changes N to A in a DirectedVote" $
                updateActionIds [] ["Test"] (Dec (DirectedVote (IdList [] [N "Test"]) (IdList [] [N "Test"]) False)) `shouldBe` (Dec (DirectedVote (IdList [] [A "Test"]) (IdList [] [A "Test"]) False))
            it "changes N to A in a Uses" $
                updateActionIds [] ["Test"] (Dec (Uses (N "Test") [Act (Comp (Scored Team (IdList [] [N "Test"])))] [Act (Comp (Scored Team (IdList [] [N "Test"])))])) `shouldBe` (Dec (Uses (A "Test") [Act (Comp (Scored Team (IdList [] [A "Test"])))] [Act (Comp (Scored Team (IdList [] [A "Test"])))]))
        describe "updatePhaseIds" $ do
            it "changes N to A in an Action" $
                updatePhaseIds [] ["Test"] [Act (Comp (Scored Team (IdList [] [N "Test"])))] `shouldBe` [Act (Comp (Scored Team (IdList [] [A "Test"])))]
            it "changes N to A in an AffiliationUpdate" $
                updatePhaseIds [] ["Test"] [Prog (AU (Elimination) (IdList [] [N "Test"]))] `shouldBe` [Prog (AU (Elimination) (IdList [] [A "Test"]))]
            it "changes N to A in a CounterUpdate" $
                updatePhaseIds [] ["Test"] [Prog (CU (Set "votes" (Num 0)) (IdList [] [N "Test"]))] `shouldBe` [Prog (CU (Set "votes" (Num 0)) (IdList [] [A "Test"]))]
            it "changes N to A in multiple phases" $
                updatePhaseIds [] ["Test"] [Prog (CU (Set "votes" (Num 0)) (IdList [] [N "Test"])), Prog (AU (Elimination) (IdList [] [N "Test"]))] `shouldBe` [Prog (CU (Set "votes" (Num 0)) (IdList [] [A "Test"])), Prog (AU (Elimination) (IdList [] [A "Test"]))]
        describe "updateRoundIds" $ do
            it "changes N to A in a Round" $
                updateRoundIds [] ["Test"] (R [Prog (CU (Set "votes" (Num 0)) (IdList [] [N "Test"])), Prog (AU (Elimination) (IdList [] [N "Test"]))] 5 []) `shouldBe` (R [Prog (CU (Set "votes" (Num 0)) (IdList [] [A "Test"])), Prog (AU (Elimination) (IdList [] [A "Test"]))] 5 [])
        describe "updateIds" $ do
            it "changes N to A in a Game" $
                updateIds (G (PI [P "Brooks" [Affiliation "Test"]] [] False) [R [Prog (CU (Set "votes" (Num 0)) (IdList [] [N "Test"])), Prog (AU (Elimination) (IdList [] [N "Test"]))] 5 [], R [Act (Comp (Scored Team (IdList [] [N "Test"])))] 3 []] Survive) `shouldBe` (G (PI [P "Brooks" [Affiliation "Test"]] [] False) [R [Prog (CU (Set "votes" (Num 0)) (IdList [] [A "Test"])), Prog (AU (Elimination) (IdList [] [A "Test"]))] 5 [], R [Act (Comp (Scored Team (IdList [] [A "Test"])))] 3 []] Survive)
        describe "preCompile" $ do
            it "applies modifiers, updates competitions, and updates ids" $
                preCompile (G (PI [P "Brooks" [Affiliation "Test"]] [] False) [R [Prog (CU (Set "votes" (Num 0)) (IdList [] [N "Test"])), Prog (AU (Elimination) (IdList [] [N "Test"]))] 5 [From 4 Before 2 exPhase1], R [Act (Comp (Placed Team (IdList [] [N "Test"]) True True)), Prog (AU (Elimination) (IdList [] [Loser (CRef 0)]))] 3 []] Survive) `shouldBe` (G (PI [P "Brooks" [Affiliation "Test"]] [] False) [R [Prog (CU (Set "votes" (Num 0)) (IdList [] [A "Test"])), Prog (AU (Elimination) (IdList [] [A "Test"]))] 3 [], R [Prog (CU (Set "votes" (Num 0)) (IdList [] [A "Test"])), exPhase1, Prog (AU (Elimination) (IdList [] [A "Test"]))] 2 [], R [Act (Comp (Placed Team (IdList [] [A "Test"]) False True)), Prog (AU (Elimination) (IdList [] [Loser (CRef 0)]))] 3 []] Survive)
    describe "Compiler" $ do
        describe "compileAffiliations" $ do
            it "compiles an attribute list into a list of string affiliations" $
                compileAffiliations [Affiliation "Test", Counter "votes" Nothing Nothing Nothing, Affiliation "Great"] `shouldBe` text "[\"Test\", \"Great\"]"
            it "compiles an attribute list with no affiliations into an empty list" $
                compileAffiliations [Counter "votes" Nothing Nothing Nothing] `shouldBe` text "[]"
        describe "compileCounter" $ do
            it "compiles a counter with no starting, min, or max values" $
                compileCounter (Counter "votes" Nothing Nothing Nothing) `shouldBe` text "{\"counter\": \"votes\"}"
            it "compiles a counter with a starting value" $
                compileCounter (Counter "votes" (Just 20) Nothing Nothing) `shouldBe` text "{\"counter\": \"votes\", \"starts\": 20}"
            it "compiles a counter with a min value" $
                compileCounter (Counter "votes" Nothing (Just 0) Nothing) `shouldBe` text "{\"counter\": \"votes\", \"min\": 0}"
            it "compiles a counter with a max value" $
                compileCounter (Counter "votes" Nothing Nothing (Just 100)) `shouldBe` text "{\"counter\": \"votes\", \"max\": 100}"   
            it "compiles a counter with a max value" $
                compileCounter (Counter "votes" (Just 20) (Just 0) (Just 100)) `shouldBe` text "{\"counter\": \"votes\", \"starts\": 20, \"min\": 0, \"max\": 100}"
        describe "compileCountersFromAttList" $ do
            it "compiles counters from a list of attributes" $ do
                compileCountersFromAttList [Affiliation "Test", Counter "votes" Nothing Nothing Nothing, Counter "points" Nothing Nothing Nothing] `shouldBe` [text "{\"counter\": \"votes\"}", text "{\"counter\": \"points\"}"]
            it "compiles an empty list if no counters in attribute list" $
                compileCountersFromAttList [Affiliation "Test"] `shouldBe` []
        describe "compileCounters" $ do
            it "compiles a list of compiled counters into a python list" $
                compileCounters [text "{\"counter\": \"votes\"}", text "{\"counter\": \"points\"}"] `shouldBe` text "[{\"counter\": \"votes\"}, {\"counter\": \"points\"}]"
            it "compiles an empty list into a python empty list" $
                compileCounters [] `shouldBe` text "[]"
        describe "compilePlayer" $ do
            it "compiles a Player into a python Player object appended to the playerList" $
                compilePlayer (P "Brooks" [Affiliation "Great", Counter "votes" (Just 20) (Just 0) Nothing]) `shouldBe` text "game.playerList.append(Player(\"Brooks\", [\"Great\"], [{\"counter\": \"votes\", \"starts\": 20, \"min\": 0}]))"
        describe "compilePlayers" $ do
            it "compiles a list of Players into python code for each player on a separate line" $
                compilePlayers [P "Brooks" [], P "Test" []] `shouldBe` text "game.playerList.append(Player(\"Brooks\", [], []))\ngame.playerList.append(Player(\"Test\", [], []))"
        describe "compileTeam" $ do
            it "compiles a team Name into python code for adding the team to the teamList" $ do
                compileTeam "Jays" `shouldBe` text "game.teamList.append(\"Jays\")"
        describe "compileTeams" $ do
            it "compiles a list of teams into python code for adding each team to the teamList" $
                compileTeams ["Kucha", "Ogakor"] `shouldBe` text "game.teamList.append(\"Kucha\")\ngame.teamList.append(\"Ogakor\")"
            it "compiles no teams into nothing" $
                compileTeams [] `shouldBe` empty
        describe "compilePlayerInfo" $ do
            it "compiles a PlayerInfo by adding players and teams to playerList and teamList and not randomly dividing teams when False" $
                compilePlayerInfo (PI [P "Brooks" [], P "Mac" []] ["Kucha", "Ogakor"] False) `shouldBe` text 
                    "game.playerList.append(Player(\"Brooks\", [], []))\ngame.playerList.append(Player(\"Mac\", [], []))\ngame.teamList.append(\"Kucha\")\ngame.teamList.append(\"Ogakor\")"
            it "compiles a PlayerInfo by adding players and teams to playerList and teamList and randomly dividing teams when True" $
                compilePlayerInfo (PI [P "Brooks" [], P "Mac" []] ["Kucha", "Ogakor"] True) `shouldBe` text 
                "game.playerList.append(Player(\"Brooks\", [], []))\ngame.playerList.append(Player(\"Mac\", [], []))\ngame.teamList.append(\"Kucha\")\ngame.teamList.append(\"Ogakor\")\nrandomlyDivideTeams(game.teamList, game.playerList)"
        describe "compileCompRef" $ do
            it "should compile a CompRef into an access of compResults" $
                compileCompRef (CRef 3) `shouldBe` text "game.compResults[2]"
        describe "compileVoteRef" $ do
            it "should compile a VoteRef into an access of voteResults" $
                compileVoteRef (VRef 0) `shouldBe` text "game.voteResults[-1]"
        describe "compileAllocRef" $ do
            it "should compile an AllocRef into an access of allocateResults" $
                compileAllocRef (ARef 1) `shouldBe` text "game.allocateResults[0]"
        describe "compileValue" $ do
            it "compiles a Num into a regular number" $
                runReader (compileValue (Num 3)) ids `shouldBe` text "3"
            it "compiles a negative Num into a negative number" $
                runReader (compileValue (Num (-3))) ids `shouldBe` text "-3"
            it "compiles a Count into an access of a player's counter" $
                runReader (compileValue (Count "points")) ids `shouldBe` text "player.counters[\"points\"]"
            it "compiles a Result into an access of the compResults array scores" $
                runReader (compileValue (Result (Cmp (CRef 0)))) ids `shouldBe` text "game.compResults[-1][\"scores\"][player] if player in game.compResults[-1][\"scores\"].keys() else game.compResults[-1][\"scores\"][[x for x in game.teamList if x in player.affiliations][0]]"
            it "compiles a Result into an access of the voteResults array votes" $
                runReader (compileValue (Result (Vt (VRef 0)))) ids `shouldBe` text "game.voteResults[-1][player]"
            it "compiles a Result into an access of the allocateResults array allocated" $
                runReader (compileValue (Result (Alloc (ARef 0)))) ids `shouldBe` text "game.allocateResults[-1][player]"
        describe "compileIdentifier" $ do
            it "compiles Everyone into the playerList" $
                runReader (compileIdentifier Everyone 1) ids `shouldBe` (text "ident = game.playerList", [])
            it "compiles Chance into a call to randomDraw" $
                runReader (compileIdentifier (Chance (IdList [IdVal Everyone (Num 2)] [N "Brooks"])) 1) ids `shouldBe` (text "includeList2 = []\nident = game.playerList\nidVal = []\nfor player in ident: idVal += [player] * 2\nincludeList2 += idVal\nexcludeList2 = []\nident = [x for x in game.playerList if x.name == \"Brooks\"]\nexcludeList2 += ident\nidList2 = [x for x in includeList2 if x not in excludeList2]\nident = [randomDraw(idList2)]", [])
            it "compiles Nominated into a list of nominated players" $
                runReader (compileIdentifier Nominated 1) ids `shouldBe` (text "ident = [x for x in game.playerList if \"nominee\" in x.affiliations]", [])
            it "compiles Tied into local variable tied for tiebreaker functions" $
                runReader (compileIdentifier Tied 1) ids `shouldBe` (text "ident = tied", [])
            it "compiles Eliminated into the eliminated players list" $
                runReader (compileIdentifier Eliminated 1) ids `shouldBe` (text "ident = eliminated", [])
            it "compiles N into a list containing the named player" $
                runReader (compileIdentifier (N "Brooks") 1) ids `shouldBe` (text "ident = [x for x in game.playerList if x.name == \"Brooks\"]", [])
            it "compiles A into a list containing all players with the specified affiliation" $
                runReader (compileIdentifier (A "Test") 1) ids `shouldBe` (text "ident = [x for x in game.playerList if \"Test\" in x.affiliations]", [])
            it "compiles Winner into a list containing the winning player or all players with the winning affiliation" $
                runReader (compileIdentifier (Winner (CRef 0)) 1) ids `shouldBe` (text "ident = [x for x in game.playerList if x == game.compResults[-1][\"winner\"] or game.compResults[-1][\"winner\"] in x.affiliations]", [])
            it "compiles Loser into a list containing the losing player or all players with the losing affiliation" $
                runReader (compileIdentifier (Loser (CRef 0)) 1) ids `shouldBe` (text "ident = [x for x in game.playerList if x == game.compResults[-1][\"loser\"] or game.compResults[-1][\"loser\"] in x.affiliations]", [])
            it "compiles Majority vote receiver with no tiebreaker into a list containing the majority vote receiver" $
                runReader (compileIdentifier (Majority (VRef 0) Nothing) 1) ids `shouldBe` (text "ident = [getVoteMinOrMax(game.voteResults[-1], True)]", [])
            it "compiles Majority vote receiver with a tiebreaker into a list containing the majority vote receiver" $
                runReader (compileIdentifier (Majority (VRef 0) (Just (Tiebreak "rocks" Nothing Tied))) 1) ids `shouldBe` (text "ident = [getVoteMinOrMax(game.voteResults[-1], True, rocksTiebreaker)]", [text "def rocksTiebreaker(tied):\n    ident = tied\n    return ident[0]"])
            it "compiles Minority vote receiver with no tiebreaker into a list containing the minority vote receiver" $
                runReader (compileIdentifier (Minority (VRef 0) Nothing) 1) ids `shouldBe` (text "ident = [getVoteMinOrMax(game.voteResults[-1], False)]", [])
            it "compiles Minority vote receiver with a tiebreaker into a list containing the minority vote receiver" $
                runReader (compileIdentifier (Minority (VRef 0) (Just (Tiebreak "rocks" Nothing Tied))) 1) ids `shouldBe` (text "ident = [getVoteMinOrMax(game.voteResults[-1], False, rocksTiebreaker)]", [text "def rocksTiebreaker(tied):\n    ident = tied\n    return ident[0]"])
            it "compiles a Most with no tiebreaker into a list containing the player with the most of the counter" $
                runReader (compileIdentifier (Most "votes" (IdList [] [Everyone]) Nothing) 1) ids `shouldBe` (text "includeList2 = []\nexcludeList2 = []\nident = game.playerList\nexcludeList2 += ident\nidList2 = [x for x in includeList2 if x not in excludeList2]\nident = [getMinOrMax(idList2, \"votes\", True)]", [])
            it "compiles a Most with a tiebreaker into a list containing the player with the most of the counter" $
                runReader (compileIdentifier (Most "votes" (IdList [] [Everyone]) (Just (Tiebreak "rocks" Nothing Tied))) 1) ids `shouldBe` (text "includeList2 = []\nexcludeList2 = []\nident = game.playerList\nexcludeList2 += ident\nidList2 = [x for x in includeList2 if x not in excludeList2]\nident = [getMinOrMax(idList2, \"votes\", True, rocksTiebreaker)]", [text "def rocksTiebreaker(tied):\n    ident = tied\n    return ident[0]"])
            it "compiles a Least with no tiebreaker into a list containing the player with the most of the counter" $
                runReader (compileIdentifier (Least "votes" (IdList [] [Everyone]) Nothing) 1) ids `shouldBe` (text "includeList2 = []\nexcludeList2 = []\nident = game.playerList\nexcludeList2 += ident\nidList2 = [x for x in includeList2 if x not in excludeList2]\nident = [getMinOrMax(idList2, \"votes\", False)]", [])
            it "compiles a Least with a tiebreaker into a list containing the player with the least of the counter" $
                runReader (compileIdentifier (Least "votes" (IdList [] [Everyone]) (Just (Tiebreak "rocks" Nothing Tied))) 1) ids `shouldBe` (text "includeList2 = []\nexcludeList2 = []\nident = game.playerList\nexcludeList2 += ident\nidList2 = [x for x in includeList2 if x not in excludeList2]\nident = [getMinOrMax(idList2, \"votes\", False, rocksTiebreaker)]", [text "def rocksTiebreaker(tied):\n    ident = tied\n    return ident[0]"])
        describe "compileIdentifiers" $ do
            it "compiles an empty list of identifiers as an empty excludeList" $
                runReader (compileIdentifiers [] 1) ids `shouldBe` (text "excludeList1 = []", [])
            it "compiles a multiple-element list of identifiers and appends each to excludeList" $
                runReader (compileIdentifiers [N "Brooks", Everyone] 1) ids `shouldBe` (text "excludeList1 = []\nident = [x for x in game.playerList if x.name == \"Brooks\"]\nexcludeList1 += ident\nident = game.playerList\nexcludeList1 += ident", [])
        describe "compileIdVal" $ do
            it "compiles an IdentifierVal with Value Num 1 just by assigning the Identifier to idVal variable" $
                runReader (compileIdVal (IdVal Everyone (Num 1)) 1) ids `shouldBe` (text "ident = game.playerList\nidVal = ident", [])
            it "compiles any other IdentifierVal by repeating each Identifier Value times" $
                runReader (compileIdVal (IdVal Everyone (Num 2)) 1) ids `shouldBe` (text "ident = game.playerList\nidVal = []\nfor player in ident: idVal += [player] * 2", [])
        describe "compileIdVals" $ do
            it "compiles an empty list of IdentifierVals by assigning includeList to be empty" $
                runReader (compileIdVals [] 1) ids `shouldBe` (text "includeList1 = []", [])
            it "compiles a non-empty IdentifierVal list by appending each compiled IdentifierVal onto the includeList" $
                runReader (compileIdVals [IdVal Everyone (Num 1), IdVal (N "Brooks") (Count "votes")] 1) ids `shouldBe` (text "includeList1 = []\nident = game.playerList\nidVal = ident\nincludeList1 += idVal\nident = [x for x in game.playerList if x.name == \"Brooks\"]\nidVal = []\nfor player in ident: idVal += [player] * player.counters[\"votes\"]\nincludeList1 += idVal", [])
        describe "compileIdentifierList" $ do
            it "compiles an IdentifierList by filtering the excludeList from the includeList" $
                runReader (compileIdentifierList (IdList [IdVal Everyone (Num 1)] [N "Brooks"]) 1) ids `shouldBe` (text "includeList1 = []\nident = game.playerList\nidVal = ident\nincludeList1 += idVal\nexcludeList1 = []\nident = [x for x in game.playerList if x.name == \"Brooks\"]\nexcludeList1 += ident\nidList1 = [x for x in includeList1 if x not in excludeList1]", [])
        describe "compileComp" $ do
            it "compiles a Scored Team Competition into a call to a python function to get the competition results" $
                runReader (compileComp (Scored Team (IdList [IdVal Everyone (Num 1)] []))) ids `shouldBe` (text "includeList1 = []\nident = game.playerList\nidVal = ident\nincludeList1 += idVal\nexcludeList1 = []\nidList1 = [x for x in includeList1 if x not in excludeList1]\ngame.getScoredTeamCompResults(list(dict.fromkeys([x for x in game.teamList for y in idList1 if x in y.affiliations])))", [])
            it "compiles a Scored Individual Competition into a call to a python function to get the competition results" $
                runReader (compileComp (Scored Individual (IdList [IdVal Everyone (Num 1)] []))) ids `shouldBe` (text "includeList1 = []\nident = game.playerList\nidVal = ident\nincludeList1 += idVal\nexcludeList1 = []\nidList1 = [x for x in includeList1 if x not in excludeList1]\ngame.getScoredCompResults(idList1)", [])
            it "compiles a Placed Team Competition into a call to a python function to get the competition results" $
                runReader (compileComp (Placed Team (IdList [IdVal Everyone (Num 1)] []) True False)) ids `shouldBe` (text "includeList1 = []\nident = game.playerList\nidVal = ident\nincludeList1 += idVal\nexcludeList1 = []\nidList1 = [x for x in includeList1 if x not in excludeList1]\ngame.getTeamCompResults(list(dict.fromkeys([x for x in game.teamList for y in idList1 if x in y.affiliations])), True, False)", [])
            it "compiles a Placed Individual Competition into a call to a python function to get the competition results" $
                runReader (compileComp (Placed Individual (IdList [IdVal Everyone (Num 1)] []) True False)) ids `shouldBe` (text "includeList1 = []\nident = game.playerList\nidVal = ident\nincludeList1 += idVal\nexcludeList1 = []\nidList1 = [x for x in includeList1 if x not in excludeList1]\ngame.getCompResults(idList1, True, False)", [])
        describe "compileDec" $ do
            it "compiles a Vote into a call to a python function to get the vote results" $
                runReader (compileDec (Vote (IdList [IdVal Everyone (Num 1)] [N "Brooks"]) (IdList [IdVal Everyone (Num 1)] [A "Test"]) False)) ids `shouldBe` (text "includeList1 = []\nident = game.playerList\nidVal = ident\nincludeList1 += idVal\nexcludeList1 = []\nident = [x for x in game.playerList if x.name == \"Brooks\"]\nexcludeList1 += ident\nidList1 = [x for x in includeList1 if x not in excludeList1]\nvoterList = idList1\nincludeList1 = []\nident = game.playerList\nidVal = ident\nincludeList1 += idVal\nexcludeList1 = []\nident = [x for x in game.playerList if \"Test\" in x.affiliations]\nexcludeList1 += ident\nidList1 = [x for x in includeList1 if x not in excludeList1]\nvoteeList = idList1\ngame.vote(voterList, voteeList, False)", [])
            it "compiles a Nomination into a call to a python function to get the nomination results and add the nomination affiliation to the nominated players" $
                runReader (compileDec (Nomination 2 (IdList [IdVal Everyone (Num 1)] [N "Brooks"]) (IdList [IdVal Everyone (Num 1)] [A "Test"]) False)) ids `shouldBe` (text "includeList1 = []\nident = game.playerList\nidVal = ident\nincludeList1 += idVal\nexcludeList1 = []\nident = [x for x in game.playerList if x.name == \"Brooks\"]\nexcludeList1 += ident\nidList1 = [x for x in includeList1 if x not in excludeList1]\nnommerList = idList1\nincludeList1 = []\nident = game.playerList\nidVal = ident\nincludeList1 += idVal\nexcludeList1 = []\nident = [x for x in game.playerList if \"Test\" in x.affiliations]\nexcludeList1 += ident\nidList1 = [x for x in includeList1 if x not in excludeList1]\npoolList = idList1\ngame.nominate(nommerList, 2, poolList, False)", [])
            it "compiles an Allocation into a call to a python function to get the allocation results and subtract the allocated amount from each player's counter" $
                runReader (compileDec (Allocation "votes" (IdList [IdVal Everyone (Num 1)] [N "Brooks"]))) ids `shouldBe` (text "includeList1 = []\nident = game.playerList\nidVal = ident\nincludeList1 += idVal\nexcludeList1 = []\nident = [x for x in game.playerList if x.name == \"Brooks\"]\nexcludeList1 += ident\nidList1 = [x for x in includeList1 if x not in excludeList1]\ngame.allocate(idList1, \"votes\")", [])
            it "compiles a DirectedVote into a call to a python function to get the vote results" $
                runReader (compileDec (DirectedVote (IdList [IdVal Everyone (Num 1)] [N "Brooks"]) (IdList [IdVal Everyone (Num 1)] [A "Test"]) False)) ids `shouldBe` (text "includeList1 = []\nident = game.playerList\nidVal = ident\nincludeList1 += idVal\nexcludeList1 = []\nident = [x for x in game.playerList if x.name == \"Brooks\"]\nexcludeList1 += ident\nidList1 = [x for x in includeList1 if x not in excludeList1]\nvoterList = idList1\nincludeList1 = []\nident = game.playerList\nidVal = ident\nincludeList1 += idVal\nexcludeList1 = []\nident = [x for x in game.playerList if \"Test\" in x.affiliations]\nexcludeList1 += ident\nidList1 = [x for x in includeList1 if x not in excludeList1]\nvoteeList = idList1\ngame.directedVote(voterList, voteeList, False)", [])
            it "compiles a Uses with no 'otherwise' phase list into a call to a python function to get the decision results and a conditional based on the result" $
                runReader (compileDec (Uses (N "Brooks") [Act (Dec (Vote (IdList [] [Everyone]) (IdList [IdVal Everyone (Num 1)] []) False))] [])) ids `shouldBe` (text "ident = [x for x in game.playerList if x.name == \"Brooks\"]\nif uses(ident):\n    includeList1 = []\n    excludeList1 = []\n    ident = game.playerList\n    excludeList1 += ident\n    idList1 = [x for x in includeList1 if x not in excludeList1]\n    voterList = idList1\n    includeList1 = []\n    ident = game.playerList\n    idVal = ident\n    includeList1 += idVal\n    excludeList1 = []\n    idList1 = [x for x in includeList1 if x not in excludeList1]\n    voteeList = idList1\n    game.vote(voterList, voteeList, False)\n    game.voteResults = game.voteResults[0:len(game.voteResults)-1]", [])
            it "compiles a Uses with an 'otherwise' phase list into a call to a python function to get the decision results and a conditional based on the result" $
                runReader (compileDec (Uses (N "Brooks") [Act (Comp (Scored Individual (IdList [] [Everyone])))] [Act (Dec (Allocation "votes" (IdList [IdVal Everyone (Num 1)] [])))])) ids `shouldBe` (text "ident = [x for x in game.playerList if x.name == \"Brooks\"]\nif uses(ident):\n    includeList1 = []\n    excludeList1 = []\n    ident = game.playerList\n    excludeList1 += ident\n    idList1 = [x for x in includeList1 if x not in excludeList1]\n    game.getScoredCompResults(idList1)\n    game.compResults = game.compResults[0:len(game.compResults)-1]\nelse:\n    includeList1 = []\n    ident = game.playerList\n    idVal = ident\n    includeList1 += idVal\n    excludeList1 = []\n    idList1 = [x for x in includeList1 if x not in excludeList1]\n    game.allocate(idList1, \"votes\")\n    game.allocateResults = game.allocateResults[0:len(game.allocateResults)-1]", [])
        describe "compileAction" $ do
            it "compiles a competition" $
                runReader (compileAction (Comp (Scored Individual (IdList [IdVal Everyone (Num 1)] [])))) ids `shouldBe` (text "includeList1 = []\nident = game.playerList\nidVal = ident\nincludeList1 += idVal\nexcludeList1 = []\nidList1 = [x for x in includeList1 if x not in excludeList1]\ngame.getScoredCompResults(idList1)", [])
            it "compiles a decision" $
                runReader (compileAction (Dec (Allocation "votes" (IdList [IdVal Everyone (Num 1)] [N "Brooks"])))) ids `shouldBe` (text "includeList1 = []\nident = game.playerList\nidVal = ident\nincludeList1 += idVal\nexcludeList1 = []\nident = [x for x in game.playerList if x.name == \"Brooks\"]\nexcludeList1 += ident\nidList1 = [x for x in includeList1 if x not in excludeList1]\ngame.allocate(idList1, \"votes\")", [])
        describe "compileTiebreaker" $ do
            it "compiles a Tiebreaker with no action into a function name and definition" $
                runReader (compileTiebreaker (Tiebreak "rocks" Nothing Tied) 1) ids `shouldBe` (text "rocksTiebreaker", [text "def rocksTiebreaker(tied):\n    ident = tied\n    return ident[0]"])
            it "compiles a Tiebreaker with an action into a function name and definition" $
                runReader (compileTiebreaker (Tiebreak "rocks" (Just (Dec (Nomination 2 (IdList [IdVal Everyone (Num 1)] [N "Brooks"]) (IdList [IdVal Everyone (Num 1)] [A "Test"]) False))) Nominated) 1) ids `shouldBe` (text "rocksTiebreaker", [text "def rocksTiebreaker(tied):\n    includeList1 = []\n    ident = game.playerList\n    idVal = ident\n    includeList1 += idVal\n    excludeList1 = []\n    ident = [x for x in game.playerList if x.name == \"Brooks\"]\n    excludeList1 += ident\n    idList1 = [x for x in includeList1 if x not in excludeList1]\n    nommerList = idList1\n    includeList1 = []\n    ident = game.playerList\n    idVal = ident\n    includeList1 += idVal\n    excludeList1 = []\n    ident = [x for x in game.playerList if \"Test\" in x.affiliations]\n    excludeList1 += ident\n    idList1 = [x for x in includeList1 if x not in excludeList1]\n    poolList = idList1\n    game.nominate(nommerList, 2, poolList, False)\n    ident = [x for x in game.playerList if \"nominee\" in x.affiliations]\n    return ident[0]"])
            it "compiles a Tiebreaker with an action into a function name and definition and deletes result if action was a Vote" $
                runReader (compileTiebreaker (Tiebreak "rocks" (Just (Dec (Vote (IdList [IdVal Everyone (Num 1)] [N "Brooks"]) (IdList [IdVal Everyone (Num 1)] [A "Test"]) False))) Nominated) 1) ids `shouldBe` (text "rocksTiebreaker", [text "def rocksTiebreaker(tied):\n    includeList1 = []\n    ident = game.playerList\n    idVal = ident\n    includeList1 += idVal\n    excludeList1 = []\n    ident = [x for x in game.playerList if x.name == \"Brooks\"]\n    excludeList1 += ident\n    idList1 = [x for x in includeList1 if x not in excludeList1]\n    voterList = idList1\n    includeList1 = []\n    ident = game.playerList\n    idVal = ident\n    includeList1 += idVal\n    excludeList1 = []\n    ident = [x for x in game.playerList if \"Test\" in x.affiliations]\n    excludeList1 += ident\n    idList1 = [x for x in includeList1 if x not in excludeList1]\n    voteeList = idList1\n    game.vote(voterList, voteeList, False)\n    ident = [x for x in game.playerList if \"nominee\" in x.affiliations]\n    del game.voteResults[-1]\n    return ident[0]"])
            it "compiles a Tiebreaker with an action into a function name and definition and deletes result if action was a DirectedVote" $
                runReader (compileTiebreaker (Tiebreak "rocks" (Just (Dec (DirectedVote (IdList [IdVal Everyone (Num 1)] [N "Brooks"]) (IdList [IdVal Everyone (Num 1)] [A "Test"]) False))) Nominated) 1) ids `shouldBe` (text "rocksTiebreaker", [text "def rocksTiebreaker(tied):\n    includeList1 = []\n    ident = game.playerList\n    idVal = ident\n    includeList1 += idVal\n    excludeList1 = []\n    ident = [x for x in game.playerList if x.name == \"Brooks\"]\n    excludeList1 += ident\n    idList1 = [x for x in includeList1 if x not in excludeList1]\n    voterList = idList1\n    includeList1 = []\n    ident = game.playerList\n    idVal = ident\n    includeList1 += idVal\n    excludeList1 = []\n    ident = [x for x in game.playerList if \"Test\" in x.affiliations]\n    excludeList1 += ident\n    idList1 = [x for x in includeList1 if x not in excludeList1]\n    voteeList = idList1\n    game.directedVote(voterList, voteeList, False)\n    ident = [x for x in game.playerList if \"nominee\" in x.affiliations]\n    del game.voteResults[-1]\n    return ident[0]"])
            it "compiles a Tiebreaker with an action into a function name and definition and deletes result if action was an Allocation" $
                runReader (compileTiebreaker (Tiebreak "rocks" (Just (Dec (Allocation "votes" (IdList [IdVal Everyone (Num 1)] [N "Brooks"])))) Nominated) 1) ids `shouldBe` (text "rocksTiebreaker", [text "def rocksTiebreaker(tied):\n    includeList1 = []\n    ident = game.playerList\n    idVal = ident\n    includeList1 += idVal\n    excludeList1 = []\n    ident = [x for x in game.playerList if x.name == \"Brooks\"]\n    excludeList1 += ident\n    idList1 = [x for x in includeList1 if x not in excludeList1]\n    game.allocate(idList1, \"votes\")\n    ident = [x for x in game.playerList if \"nominee\" in x.affiliations]\n    del game.allocateResults[-1]\n    return ident[0]"])
            it "compiles a Tiebreaker with an action into a function name and definition and deletes result if action was a Competition" $
                runReader (compileTiebreaker (Tiebreak "rocks" (Just (Comp (Scored Individual (IdList [IdVal Everyone (Num 1)] [])))) Nominated) 1) ids `shouldBe` (text "rocksTiebreaker", [text "def rocksTiebreaker(tied):\n    includeList1 = []\n    ident = game.playerList\n    idVal = ident\n    includeList1 += idVal\n    excludeList1 = []\n    idList1 = [x for x in includeList1 if x not in excludeList1]\n    game.getScoredCompResults(idList1)\n    ident = [x for x in game.playerList if \"nominee\" in x.affiliations]\n    del game.compResults[-1]\n    return ident[0]"])
        describe "compileNameList" $ do
            it "compiles an empty list of names" $
                compileNameList [] `shouldBe` text "[]"
            it "compiles a singleton list of names" $
                compileNameList ["Test"] `shouldBe` text "[\"Test\"]"
            it "compiles a multi-element list of names" $
                compileNameList ["Test", "Great"] `shouldBe` text "[\"Test\", \"Great\"]"
        describe "compileAffUpdate" $ do
            it "compiles an Elimination" $
                runReader (compileAffUpdate Elimination) ids `shouldBe` text "game.eliminate(idList1)"
            it "compiles an affiliation Add" $
                runReader (compileAffUpdate (Add "Mediocre")) ids `shouldBe` text "for player in idList1: player.addAff(\"Mediocre\")"
            it "compiles an affiliation Remove" $
                runReader (compileAffUpdate (Remove "Test")) ids `shouldBe` text "for player in idList1: player.removeAff(\"Test\")"
            it "compiles an affiliation Change" $
                runReader (compileAffUpdate (Change "Test" "Mediocre")) ids `shouldBe` text "for player in idList1: player.removeAff(\"Test\"); player.addAff(\"Mediocre\")"
            it "compiles an affiliation Swap" $
                runReader (compileAffUpdate (Swap ["Test", "Great"] ["Mediocre"] False)) ids `shouldBe` text "game.swap([\"Test\", \"Great\"], [\"Mediocre\"], idList1, False)"
            it "compiles an affiliation Merge with no merge name" $
                runReader (compileAffUpdate (Merge ["Test", "Great"] Nothing)) ids `shouldBe` text "game.merge([\"Test\", \"Great\"], \"merged\", idList1)"
            it "compiles an affiliation Merge with a merge name" $
                runReader (compileAffUpdate (Merge ["Test", "Great"] (Just "Mediocre"))) ids `shouldBe` text "game.merge([\"Test\", \"Great\"], \"Mediocre\", idList1)"
        describe "compileCounterUpdate" $ do
            it "compiles an Increase counter update" $
                runReader (compileCounterUpdate (Increase "votes" (Num 3))) ids `shouldBe` text "for player in idList1: player.updateCounter(3, \"votes\")"
            it "compiles a Decrease counter update" $
                runReader (compileCounterUpdate (Decrease "votes" (Num 3))) ids `shouldBe` text "for player in idList1: player.updateCounter(- (3), \"votes\")"
            it "compiles a Set counter update" $
                runReader (compileCounterUpdate (Set "votes" (Num 0))) ids `shouldBe` text "for player in idList1: player.setCounter(0, \"votes\")"
        describe "compileProgression" $ do
            it "compiles an AU Progression" $
                runReader (compileProgression (AU Elimination (IdList [] []))) ids `shouldBe` (text "includeList1 = []\nexcludeList1 = []\nidList1 = [x for x in includeList1 if x not in excludeList1]\ngame.eliminate(idList1)", [])
            it "compiles a CU Progression" $
                runReader (compileProgression (CU (Increase "votes" (Num 1)) (IdList [] []))) ids `shouldBe` (text "includeList1 = []\nexcludeList1 = []\nidList1 = [x for x in includeList1 if x not in excludeList1]\nfor player in idList1: player.updateCounter(1, \"votes\")", [])
        describe "compilePhaseList" $ do
            it "compiles an empty phase list" $ do
                runReader (compilePhaseList []) ids `shouldBe` (empty, [])
            it "compiles a non-empty phase list" $ do
                runReader (compilePhaseList [Act (Comp (Scored Individual (IdList [IdVal Everyone (Num 1)] []))), Prog (AU Elimination (IdList [] []))]) ids `shouldBe` (text "includeList1 = []\nident = game.playerList\nidVal = ident\nincludeList1 += idVal\nexcludeList1 = []\nidList1 = [x for x in includeList1 if x not in excludeList1]\ngame.getScoredCompResults(idList1)\nincludeList1 = []\nexcludeList1 = []\nidList1 = [x for x in includeList1 if x not in excludeList1]\ngame.eliminate(idList1)", [])
        describe "countCompsInPhaseList" $ do
            it "counts the number of competitions in a phase list" $ do
                countCompsInPhaseList [Act (Comp (Scored Team (IdList [] []))), Prog (AU Elimination (IdList [] [])), Act (Dec (Vote (IdList [] []) (IdList [] []) False)), Act (Dec (Allocation "votes" (IdList [] []))), Act (Comp (Placed Individual (IdList [] []) False True)), Act (Dec (Nomination 2 (IdList [] []) (IdList [] []) True)), Act (Dec (DirectedVote (IdList [] []) (IdList [] []) True))] `shouldBe` 2
        describe "countVotesInPhaseList" $ do
            it "counts the number of votes in a phase list" $
                countVotesInPhaseList [Act (Comp (Scored Team (IdList [] []))), Prog (AU Elimination (IdList [] [])), Act (Dec (Vote (IdList [] []) (IdList [] []) False)), Act (Dec (Allocation "votes" (IdList [] []))), Act (Comp (Placed Individual (IdList [] []) False True)), Act (Dec (Nomination 2 (IdList [] []) (IdList [] []) True)), Act (Dec (DirectedVote (IdList [] []) (IdList [] []) True))] `shouldBe` 2
        describe "countAllocsInPhaseList" $ do
            it "counts the number of allocations in a phase list" $
                countAllocsInPhaseList [Act (Comp (Scored Team (IdList [] []))), Prog (AU Elimination (IdList [] [])), Act (Dec (Vote (IdList [] []) (IdList [] []) False)), Act (Dec (Allocation "votes" (IdList [] []))), Act (Comp (Placed Individual (IdList [] []) False True)), Act (Dec (Nomination 2 (IdList [] []) (IdList [] []) True)), Act (Dec (DirectedVote (IdList [] []) (IdList [] []) True))] `shouldBe` 1
        describe "compileRoundList" $ do
            it "compiles an empty roundList" $
                runReader (compileRoundList []) ids `shouldBe` (text "roundList =", [])
            it "compiles a single round" $ 
                runReader (compileRoundList [R [Act (Comp (Scored Individual (IdList [] [])))] 3 []]) ids `shouldBe` (text "roundList = [roundType1] * 3", [text "def roundType1():\n    includeList1 = []\n    excludeList1 = []\n    idList1 = [x for x in includeList1 if x not in excludeList1]\n    game.getScoredCompResults(idList1)"])
            it "compiles multiple rounds" $
                runReader (compileRoundList [R [Act (Comp (Scored Individual (IdList [] [])))] 3 [], R [Prog (AU Elimination (IdList [] []))] 5 []]) ids `shouldBe` (text "roundList = [roundType1] * 3 + [roundType2] * 5", [text "def roundType1():\n    includeList1 = []\n    excludeList1 = []\n    idList1 = [x for x in includeList1 if x not in excludeList1]\n    game.getScoredCompResults(idList1)", text "def roundType2():\n    includeList1 = []\n    excludeList1 = []\n    idList1 = [x for x in includeList1 if x not in excludeList1]\n    game.eliminate(idList1)"])
        describe "compileGoalList" $ do
            it "compiles an empty goal list" $
                runReader (compileGoalList []) ids `shouldBe` empty
            it "compiles a singleton goal list" $
                runReader (compileGoalList [Gl 30 "votes"]) ids `shouldBe` text "player.checkWinCondition(\"votes\", 30)"
            it "compiles a multi-element goal list" $
                runReader (compileGoalList [Gl 30 "votes", Gl 100 "points"]) ids `shouldBe` text "player.checkWinCondition(\"votes\", 30) and player.checkWinCondition(\"points\", 100)"
        describe "compileWinCondition" $ do
            it "compiles a Survive Win Condition" $
                runReader (compileWinCondition Survive) ids `shouldBe` (text "winners = \"\"\nif len(game.playerList) == 1:\n    winners = game.playerList[0].name\nelse:\n    for player in game.playerList[1:]:\n        winners += player.name + \", \"\n    winners += \"and \" + game.playerList[0].name\nprint(winners + \" won!\")", [])
            it "compiles a Jury Win Condition" $
                runReader (compileWinCondition (Jury 7)) ids `shouldBe` (text "game.juryVote(7)\nwinner = getVoteMinOrMax(game.voteResults[-1], True)\nprint(winner.name + \" won!\")", [])
            it "compiles a FinalComp Team Win Condition" $
                runReader (compileWinCondition (FinalComp Team)) ids `shouldBe` (text "game.getTeamCompResults(game.teamList, True, False)\nwinner = game.compResults[-1][\"winner\"]\nprint(winner + \" won!\")", [])
            it "compiles a FinalComp Individual Win Condition" $
                runReader (compileWinCondition (FinalComp Individual)) ids `shouldBe` (text "game.getCompResults(game.playerList, True, False)\nwinner = game.compResults[-1][\"winner\"]\nprint(winner.name + \" won!\")", [])
            it "compiles a Reach Individual Win Condition" $
                runReader (compileWinCondition (Reach [Gl 4 "points"] Individual)) ids `shouldBe` (text "    winners = []\n    for player in game.playerList:\n        if player.checkWinCondition(\"points\", 4):\n            winners.append(player.name)\n    if len(winners) > 0:\n        winnerString = \"\"\n        if len(winners) == 1:\n            winnerString = winners[0]\n        else:\n            for winner in winners[1:]:\n                winnerString += winner + \", \"\n            winnerString += \"and \" + winners[0]\n        print(winnerString + \" won!\")\n        break", [])
            it "compiles a Reach Team Win Condition" $
                runReader (compileWinCondition (Reach [Gl 4 "points"] Team)) ids `shouldBe` (text "    winners = []\n    for player in game.playerList:\n        if player.checkWinCondition(\"points\", 4):\n            winners.append(player)\n    if len(winners) > 0:\n        winTeams = list(dict.fromkeys([x for x in game.teamList for y in winners if x in y.affiliations]))\n        winnerString = \"\"\n        if len(winTeams) == 1:\n            winnerString = winTeams[0]\n        else:\n            for winner in winTeams[1:]:\n                winnerString += winner + \", \"\n            winnerString += \"and \" + winTeams[0]\n        print(winnerString + \" won!\")\n        break", [])
            it "compiles an Ids Individual Win Condition" $
                runReader (compileWinCondition (Ids (IdList [] []) Individual)) ids `shouldBe` (text "includeList1 = []\nexcludeList1 = []\nidList1 = [x for x in includeList1 if x not in excludeList1]\nwinners = \"\"\nif len(idList1) == 0: print(\"No winners!\")\nelif len(idList1) == 1:\n    winners = idList1[0].name\nelse:\n    for winner in idList1[1:]:\n        winners += winner.name + \", \"\n    winners += \"and \" + idList1[0].name\nprint(winners + \" won!\")", [])
            it "compiles an Ids Team Win Condition" $
                runReader (compileWinCondition (Ids (IdList [] []) Team)) ids `shouldBe` (text "includeList1 = []\nexcludeList1 = []\nidList1 = [x for x in includeList1 if x not in excludeList1]\nwinTeams = list(dict.fromkeys([x for x in game.teamList for y in idList1 if x in y.affiliations]))\nwinners = \"\"\nif len(winTeams) == 0: print(\"No winners!\")\nelif len(winTeams) == 1:\n    winners = winTeams[0]\nelse:\n    for winner in winTeams[1:]:\n        winners += winner + \", \"\n    winners += \"and \" + winTeams[0]\nprint(winners + \" won!\")", [])
        describe "getCountersFromAttList" $ do
            it "gets no counter names from an empty attribute list" $ 
                getCountersFromAttList [] `shouldBe` []
            it "gets counter names from populated attribute list" $
                getCountersFromAttList [Affiliation "Test", Counter "votes" (Just 20) (Just 0) Nothing, Counter "points" Nothing Nothing Nothing, Affiliation "Great"] `shouldBe` ["votes", "points"]
        describe "getCountersFromPlayerList" $ do
            it "gets no counter names from an empty player list" $
                getCountersFromPlayerList [] `shouldBe` []
            it "gets counter names from a populated player list" $
                getCountersFromPlayerList [P "Brooks" [Affiliation "Great", Counter "votes" (Just 20) (Just 0) Nothing], P "Mac" [Affiliation "Test", Counter "points" Nothing Nothing Nothing]] `shouldBe` ["votes", "points"]
        describe "getAllCounters" $
            it "gets all counter names from a Game" $
                getAllCounters (G (PI [P "Brooks" [Affiliation "Great", Counter "votes" (Just 20) (Just 0) Nothing], P "Mac" [Affiliation "Test", Counter "points" Nothing Nothing Nothing]] ["Test", "Great"] False) [] Survive) `shouldBe` ["votes", "points"]
        describe "compileGame" $
            it "compiles a complete Game" $
                compileGame (G (PI [P "Brooks" [Affiliation "Great", Counter "votes" (Just 20) (Just 0) Nothing], P "Mac" [Affiliation "Test", Counter "points" Nothing Nothing Nothing]] ["Test", "Great"] False) [R [Act (Comp (Placed Individual (IdList [IdVal Everyone (Num 1)] []) True False)), Prog (CU (Increase "points" (Num 1)) (IdList [IdVal (Winner (CRef 0)) (Num 1)] []))] 5 []] (Jury 7)) `shouldBe` text "from gamelib import *\n\ndef roundType1():\n    includeList1 = []\n    ident = game.playerList\n    idVal = ident\n    includeList1 += idVal\n    excludeList1 = []\n    idList1 = [x for x in includeList1 if x not in excludeList1]\n    game.getCompResults(idList1, True, False)\n    includeList1 = []\n    ident = [x for x in game.playerList if x == game.compResults[-1][\"winner\"] or game.compResults[-1][\"winner\"] in x.affiliations]\n    idVal = ident\n    includeList1 += idVal\n    excludeList1 = []\n    idList1 = [x for x in includeList1 if x not in excludeList1]\n    for player in idList1: player.updateCounter(1, \"points\")\n\ngame = Game()\n\ngame.playerList.append(Player(\"Brooks\", [\"Great\"], [{\"counter\": \"votes\", \"starts\": 20, \"min\": 0}]))\ngame.playerList.append(Player(\"Mac\", [\"Test\"], [{\"counter\": \"points\"}]))\ngame.teamList.append(\"Test\")\ngame.teamList.append(\"Great\")\n\nroundList = [roundType1] * 5\n\nfor round in roundList:\n    game.resetResults()\n    round()\n\ngame.juryVote(7)\nwinner = getVoteMinOrMax(game.voteResults[-1], True)\nprint(winner.name + \" won!\")"