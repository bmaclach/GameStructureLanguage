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

