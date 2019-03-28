import Test.Hspec
import Parser
import AST

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
            it "parses chance with an identifier list" $
                parseGame identifierP "chance (everyone)" `shouldBe` Chance (IdList [IdVal Everyone (Num 1)] [])
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
            it "parses a highest counter reference" $
                parseGame identifierP "highest points (everyone)" `shouldBe` Most "points" (IdList [IdVal Everyone (Num 1)] []) Nothing
            it "parses a most counter reference" $
                parseGame identifierP "most points (everyone)" `shouldBe` Most "points" (IdList [IdVal Everyone (Num 1)] []) Nothing
            it "parses a lowest counter reference" $
                parseGame identifierP "lowest points (everyone)" `shouldBe` Least "points" (IdList [IdVal Everyone (Num 1)] []) Nothing
            it "parses a least counter reference" $
                parseGame identifierP "least points (everyone)" `shouldBe` Least "points" (IdList [IdVal Everyone (Num 1)] []) Nothing
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
        


