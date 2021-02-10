module CompilerSpec (spec) where

import Test.Hspec

import AST
import Compiler

import Text.PrettyPrint.HughesPJ (empty, nest, text)
import Control.Monad.Reader (runReader)

ids :: IdNames
ids = IdNames ["Brooks", "Mac"] ["Test", "Great"] ["votes", "points"]

spec :: Spec
spec = do
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
    it "compiles a team Name into python code for adding the team to the teamList" $
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
      runReader (compileIdentifier Everyone 1) ids `shouldBe` text "ident = game.playerList"
    it "compiles Chance into a call to randomDraw" $
      runReader (compileIdentifier (Chance 1 (IdList [IdVal Everyone (Num 2)] [N "Brooks"])) 1) ids `shouldBe` text "includeList2 = []\nident = game.playerList\nidVal = []\nfor player in ident: idVal += [player] * 2\nincludeList2 += idVal\nexcludeList2 = []\nident = [x for x in game.playerList if x.name == \"Brooks\"]\nexcludeList2 += ident\nidList2 = [x for x in includeList2 if x not in excludeList2]\nident = randomDraw(1, idList2)"
    it "compiles Nominated into a list of nominated players" $
      runReader (compileIdentifier Nominated 1) ids `shouldBe` text "ident = [x for x in game.playerList if \"nominee\" in x.affiliations]"
    it "compiles Tied into local variable tied for tiebreaker functions" $
      runReader (compileIdentifier Tied 1) ids `shouldBe` text "ident = tied"
    it "compiles Eliminated into the eliminated players list" $
      runReader (compileIdentifier Eliminated 1) ids `shouldBe` text "ident = eliminated"
    it "compiles N into a list containing the named player" $
      runReader (compileIdentifier (N "Brooks") 1) ids `shouldBe` text "ident = [x for x in game.playerList if x.name == \"Brooks\"]"
    it "compiles A into a list containing all players with the specified affiliation" $
      runReader (compileIdentifier (A "Test") 1) ids `shouldBe` text "ident = [x for x in game.playerList if \"Test\" in x.affiliations]"
    it "compiles Winner into a list containing the winning player or all players with the winning affiliation" $
      runReader (compileIdentifier (Winner (CRef 0)) 1) ids `shouldBe` text "ident = [x for x in game.playerList if x == game.compResults[-1][\"winner\"] or game.compResults[-1][\"winner\"] in x.affiliations]"
    it "compiles Loser into a list containing the losing player or all players with the losing affiliation" $
      runReader (compileIdentifier (Loser (CRef 0)) 1) ids `shouldBe` text "ident = [x for x in game.playerList if x == game.compResults[-1][\"loser\"] or game.compResults[-1][\"loser\"] in x.affiliations]"
    it "compiles Majority vote receiver with no tiebreaker into a list containing the majority vote receiver" $
      runReader (compileIdentifier (Majority (VRef 0) Nothing) 1) ids `shouldBe` text "ident = [getVoteMinOrMax(game.voteResults[-1], True)]"
    it "compiles Majority vote receiver with a tiebreaker into a list containing the majority vote receiver" $
      runReader (compileIdentifier (Majority (VRef 0) (Just (TieRef "rocks"))) 1) ids `shouldBe` text "ident = [getVoteMinOrMax(game.voteResults[-1], True, rocksTiebreaker)]"
    it "compiles Minority vote receiver with no tiebreaker into a list containing the minority vote receiver" $
      runReader (compileIdentifier (Minority (VRef 0) Nothing) 1) ids `shouldBe` text "ident = [getVoteMinOrMax(game.voteResults[-1], False)]"
    it "compiles Minority vote receiver with a tiebreaker into a list containing the minority vote receiver" $
      runReader (compileIdentifier (Minority (VRef 0) (Just (TieRef "rocks"))) 1) ids `shouldBe` text "ident = [getVoteMinOrMax(game.voteResults[-1], False, rocksTiebreaker)]"
    it "compiles a Most with no tiebreaker into a list containing the player with the most of the counter" $
      runReader (compileIdentifier (Most "votes" (IdList [] [Everyone]) Nothing) 1) ids `shouldBe` text "includeList2 = []\nexcludeList2 = []\nident = game.playerList\nexcludeList2 += ident\nidList2 = [x for x in includeList2 if x not in excludeList2]\nident = [getMinOrMax(idList2, \"votes\", True)]"
    it "compiles a Most with a tiebreaker into a list containing the player with the most of the counter" $
      runReader (compileIdentifier (Most "votes" (IdList [] [Everyone]) (Just (TieRef "rocks"))) 1) ids `shouldBe` text "includeList2 = []\nexcludeList2 = []\nident = game.playerList\nexcludeList2 += ident\nidList2 = [x for x in includeList2 if x not in excludeList2]\nident = [getMinOrMax(idList2, \"votes\", True, rocksTiebreaker)]"
    it "compiles a Least with no tiebreaker into a list containing the player with the most of the counter" $
      runReader (compileIdentifier (Least "votes" (IdList [] [Everyone]) Nothing) 1) ids `shouldBe` text "includeList2 = []\nexcludeList2 = []\nident = game.playerList\nexcludeList2 += ident\nidList2 = [x for x in includeList2 if x not in excludeList2]\nident = [getMinOrMax(idList2, \"votes\", False)]"
    it "compiles a Least with a tiebreaker into a list containing the player with the least of the counter" $
      runReader (compileIdentifier (Least "votes" (IdList [] [Everyone]) (Just (TieRef "rocks"))) 1) ids `shouldBe` text "includeList2 = []\nexcludeList2 = []\nident = game.playerList\nexcludeList2 += ident\nidList2 = [x for x in includeList2 if x not in excludeList2]\nident = [getMinOrMax(idList2, \"votes\", False, rocksTiebreaker)]"
  describe "compileIdentifiers" $ do
    it "compiles an empty list of identifiers as an empty excludeList" $
      runReader (compileIdentifiers [] 1) ids `shouldBe` text "excludeList1 = []"
    it "compiles a multiple-element list of identifiers and appends each to excludeList" $
      runReader (compileIdentifiers [N "Brooks", Everyone] 1) ids `shouldBe` text "excludeList1 = []\nident = [x for x in game.playerList if x.name == \"Brooks\"]\nexcludeList1 += ident\nident = game.playerList\nexcludeList1 += ident"
  describe "compileIdVal" $ do
    it "compiles an IdentifierVal with Value Num 1 just by assigning the Identifier to idVal variable" $
      runReader (compileIdVal (IdVal Everyone (Num 1)) 1) ids `shouldBe` text "ident = game.playerList\nidVal = ident"
    it "compiles any other IdentifierVal by repeating each Identifier Value times" $
      runReader (compileIdVal (IdVal Everyone (Num 2)) 1) ids `shouldBe` text "ident = game.playerList\nidVal = []\nfor player in ident: idVal += [player] * 2"
  describe "compileIdVals" $ do
    it "compiles an empty list of IdentifierVals by assigning includeList to be empty" $
      runReader (compileIdVals [] 1) ids `shouldBe` text "includeList1 = []"
    it "compiles a non-empty IdentifierVal list by appending each compiled IdentifierVal onto the includeList" $
      runReader (compileIdVals [IdVal Everyone (Num 1), IdVal (N "Brooks") (Count "votes")] 1) ids `shouldBe` text "includeList1 = []\nident = game.playerList\nidVal = ident\nincludeList1 += idVal\nident = [x for x in game.playerList if x.name == \"Brooks\"]\nidVal = []\nfor player in ident: idVal += [player] * player.counters[\"votes\"]\nincludeList1 += idVal"
  describe "compileIdentifierList" $ do
    it "compiles an IdentifierList by filtering the excludeList from the includeList" $
      runReader (compileIdentifierList (IdList [IdVal Everyone (Num 1)] [N "Brooks"]) 1) ids `shouldBe` text "includeList1 = []\nident = game.playerList\nidVal = ident\nincludeList1 += idVal\nexcludeList1 = []\nident = [x for x in game.playerList if x.name == \"Brooks\"]\nexcludeList1 += ident\nidList1 = [x for x in includeList1 if x not in excludeList1]"
  describe "compileComp" $ do
    it "compiles a Scored Team Competition into a call to a python function to get the competition results" $
      runReader (compileComp (Scored Team (IdList [IdVal Everyone (Num 1)] []))) ids `shouldBe` text "includeList1 = []\nident = game.playerList\nidVal = ident\nincludeList1 += idVal\nexcludeList1 = []\nidList1 = [x for x in includeList1 if x not in excludeList1]\ngame.getScoredTeamCompResults(list(dict.fromkeys([x for x in game.teamList for y in idList1 if x in y.affiliations])))"
    it "compiles a Scored Individual Competition into a call to a python function to get the competition results" $
      runReader (compileComp (Scored Individual (IdList [IdVal Everyone (Num 1)] []))) ids `shouldBe` text "includeList1 = []\nident = game.playerList\nidVal = ident\nincludeList1 += idVal\nexcludeList1 = []\nidList1 = [x for x in includeList1 if x not in excludeList1]\ngame.getScoredCompResults(idList1)"
    it "compiles a Placed Team Competition into a call to a python function to get the competition results" $
      runReader (compileComp (Placed Team (IdList [IdVal Everyone (Num 1)] []) True False)) ids `shouldBe` text "includeList1 = []\nident = game.playerList\nidVal = ident\nincludeList1 += idVal\nexcludeList1 = []\nidList1 = [x for x in includeList1 if x not in excludeList1]\ngame.getTeamCompResults(list(dict.fromkeys([x for x in game.teamList for y in idList1 if x in y.affiliations])), True, False)"
    it "compiles a Placed Individual Competition into a call to a python function to get the competition results" $
      runReader (compileComp (Placed Individual (IdList [IdVal Everyone (Num 1)] []) True False)) ids `shouldBe` text "includeList1 = []\nident = game.playerList\nidVal = ident\nincludeList1 += idVal\nexcludeList1 = []\nidList1 = [x for x in includeList1 if x not in excludeList1]\ngame.getCompResults(idList1, True, False)"
  describe "compileDec" $ do
    it "compiles a Vote into a call to a python function to get the vote results" $
      runReader (compileDec (Vote (IdList [IdVal Everyone (Num 1)] [N "Brooks"]) (IdList [IdVal Everyone (Num 1)] [A "Test"]) False)) ids `shouldBe` text "includeList1 = []\nident = game.playerList\nidVal = ident\nincludeList1 += idVal\nexcludeList1 = []\nident = [x for x in game.playerList if x.name == \"Brooks\"]\nexcludeList1 += ident\nidList1 = [x for x in includeList1 if x not in excludeList1]\nvoterList = idList1\nincludeList1 = []\nident = game.playerList\nidVal = ident\nincludeList1 += idVal\nexcludeList1 = []\nident = [x for x in game.playerList if \"Test\" in x.affiliations]\nexcludeList1 += ident\nidList1 = [x for x in includeList1 if x not in excludeList1]\nvoteeList = idList1\ngame.vote(voterList, voteeList, False)"
    it "compiles a Nomination into a call to a python function to get the nomination results and add the nomination affiliation to the nominated players" $
      runReader (compileDec (Nomination 2 (IdList [IdVal Everyone (Num 1)] [N "Brooks"]) (IdList [IdVal Everyone (Num 1)] [A "Test"]) False)) ids `shouldBe` text "includeList1 = []\nident = game.playerList\nidVal = ident\nincludeList1 += idVal\nexcludeList1 = []\nident = [x for x in game.playerList if x.name == \"Brooks\"]\nexcludeList1 += ident\nidList1 = [x for x in includeList1 if x not in excludeList1]\nnommerList = idList1\nincludeList1 = []\nident = game.playerList\nidVal = ident\nincludeList1 += idVal\nexcludeList1 = []\nident = [x for x in game.playerList if \"Test\" in x.affiliations]\nexcludeList1 += ident\nidList1 = [x for x in includeList1 if x not in excludeList1]\npoolList = idList1\ngame.nominate(nommerList, 2, poolList, False)"
    it "compiles an Allocation into a call to a python function to get the allocation results and subtract the allocated amount from each player's counter" $
      runReader (compileDec (Allocation "votes" (IdList [IdVal Everyone (Num 1)] [N "Brooks"]))) ids `shouldBe` text "includeList1 = []\nident = game.playerList\nidVal = ident\nincludeList1 += idVal\nexcludeList1 = []\nident = [x for x in game.playerList if x.name == \"Brooks\"]\nexcludeList1 += ident\nidList1 = [x for x in includeList1 if x not in excludeList1]\ngame.allocate(idList1, \"votes\")"
    it "compiles a DirectedVote into a call to a python function to get the vote results" $
      runReader (compileDec (DirectedVote (IdList [IdVal Everyone (Num 1)] [N "Brooks"]) (IdList [IdVal Everyone (Num 1)] [A "Test"]) False)) ids `shouldBe` text "includeList1 = []\nident = game.playerList\nidVal = ident\nincludeList1 += idVal\nexcludeList1 = []\nident = [x for x in game.playerList if x.name == \"Brooks\"]\nexcludeList1 += ident\nidList1 = [x for x in includeList1 if x not in excludeList1]\nvoterList = idList1\nincludeList1 = []\nident = game.playerList\nidVal = ident\nincludeList1 += idVal\nexcludeList1 = []\nident = [x for x in game.playerList if \"Test\" in x.affiliations]\nexcludeList1 += ident\nidList1 = [x for x in includeList1 if x not in excludeList1]\nvoteeList = idList1\ngame.directedVote(voterList, voteeList, False)"
    it "compiles a Uses with no 'otherwise' phase list into a call to a python function to get the decision results and a conditional based on the result" $
      runReader (compileDec (Uses (N "Brooks") [Act (Dec (Vote (IdList [] [Everyone]) (IdList [IdVal Everyone (Num 1)] []) False))] [])) ids `shouldBe` text "ident = [x for x in game.playerList if x.name == \"Brooks\"]\nif uses(ident):\n    includeList1 = []\n    excludeList1 = []\n    ident = game.playerList\n    excludeList1 += ident\n    idList1 = [x for x in includeList1 if x not in excludeList1]\n    voterList = idList1\n    includeList1 = []\n    ident = game.playerList\n    idVal = ident\n    includeList1 += idVal\n    excludeList1 = []\n    idList1 = [x for x in includeList1 if x not in excludeList1]\n    voteeList = idList1\n    game.vote(voterList, voteeList, False)\n    game.voteResults = game.voteResults[0:len(game.voteResults)-1]"
    it "compiles a Uses with an 'otherwise' phase list into a call to a python function to get the decision results and a conditional based on the result" $
      runReader (compileDec (Uses (N "Brooks") [Act (Comp (Scored Individual (IdList [] [Everyone])))] [Act (Dec (Allocation "votes" (IdList [IdVal Everyone (Num 1)] [])))])) ids `shouldBe` text "ident = [x for x in game.playerList if x.name == \"Brooks\"]\nif uses(ident):\n    includeList1 = []\n    excludeList1 = []\n    ident = game.playerList\n    excludeList1 += ident\n    idList1 = [x for x in includeList1 if x not in excludeList1]\n    game.getScoredCompResults(idList1)\n    game.compResults = game.compResults[0:len(game.compResults)-1]\nelse:\n    includeList1 = []\n    ident = game.playerList\n    idVal = ident\n    includeList1 += idVal\n    excludeList1 = []\n    idList1 = [x for x in includeList1 if x not in excludeList1]\n    game.allocate(idList1, \"votes\")\n    game.allocateResults = game.allocateResults[0:len(game.allocateResults)-1]"
  describe "compileAction" $ do
    it "compiles a competition" $
      runReader (compileAction (Comp (Scored Individual (IdList [IdVal Everyone (Num 1)] [])))) ids `shouldBe` text "includeList1 = []\nident = game.playerList\nidVal = ident\nincludeList1 += idVal\nexcludeList1 = []\nidList1 = [x for x in includeList1 if x not in excludeList1]\ngame.getScoredCompResults(idList1)"
    it "compiles a decision" $
      runReader (compileAction (Dec (Allocation "votes" (IdList [IdVal Everyone (Num 1)] [N "Brooks"])))) ids `shouldBe` text "includeList1 = []\nident = game.playerList\nidVal = ident\nincludeList1 += idVal\nexcludeList1 = []\nident = [x for x in game.playerList if x.name == \"Brooks\"]\nexcludeList1 += ident\nidList1 = [x for x in includeList1 if x not in excludeList1]\ngame.allocate(idList1, \"votes\")"
  describe "compileTiebreaker" $ do
    it "compiles a Tiebreaker with no action into a function name and definition" $
      runReader (compileTiebreaker (Tiebreak "Rocks" Nothing Tied)) ids `shouldBe` text "def rocksTiebreaker(tied):\n    ident = tied\n    return ident[0]"
    it "compiles a Tiebreaker with an action into a function name and definition" $
      runReader (compileTiebreaker (Tiebreak "rocks" (Just (Dec (Nomination 2 (IdList [IdVal Everyone (Num 1)] [N "Brooks"]) (IdList [IdVal Everyone (Num 1)] [A "Test"]) False))) Nominated)) ids `shouldBe` text "def rocksTiebreaker(tied):\n    includeList1 = []\n    ident = game.playerList\n    idVal = ident\n    includeList1 += idVal\n    excludeList1 = []\n    ident = [x for x in game.playerList if x.name == \"Brooks\"]\n    excludeList1 += ident\n    idList1 = [x for x in includeList1 if x not in excludeList1]\n    nommerList = idList1\n    includeList1 = []\n    ident = game.playerList\n    idVal = ident\n    includeList1 += idVal\n    excludeList1 = []\n    ident = [x for x in game.playerList if \"Test\" in x.affiliations]\n    excludeList1 += ident\n    idList1 = [x for x in includeList1 if x not in excludeList1]\n    poolList = idList1\n    game.nominate(nommerList, 2, poolList, False)\n    ident = [x for x in game.playerList if \"nominee\" in x.affiliations]\n    return ident[0]"
    it "compiles a Tiebreaker with an action into a function name and definition and deletes result if action was a Vote" $
      runReader (compileTiebreaker (Tiebreak "rocks" (Just (Dec (Vote (IdList [IdVal Everyone (Num 1)] [N "Brooks"]) (IdList [IdVal Everyone (Num 1)] [A "Test"]) False))) Nominated)) ids `shouldBe` text "def rocksTiebreaker(tied):\n    includeList1 = []\n    ident = game.playerList\n    idVal = ident\n    includeList1 += idVal\n    excludeList1 = []\n    ident = [x for x in game.playerList if x.name == \"Brooks\"]\n    excludeList1 += ident\n    idList1 = [x for x in includeList1 if x not in excludeList1]\n    voterList = idList1\n    includeList1 = []\n    ident = game.playerList\n    idVal = ident\n    includeList1 += idVal\n    excludeList1 = []\n    ident = [x for x in game.playerList if \"Test\" in x.affiliations]\n    excludeList1 += ident\n    idList1 = [x for x in includeList1 if x not in excludeList1]\n    voteeList = idList1\n    game.vote(voterList, voteeList, False)\n    ident = [x for x in game.playerList if \"nominee\" in x.affiliations]\n    del game.voteResults[-1]\n    return ident[0]"
    it "compiles a Tiebreaker with an action into a function name and definition and deletes result if action was a DirectedVote" $
      runReader (compileTiebreaker (Tiebreak "rocks" (Just (Dec (DirectedVote (IdList [IdVal Everyone (Num 1)] [N "Brooks"]) (IdList [IdVal Everyone (Num 1)] [A "Test"]) False))) Nominated)) ids `shouldBe` text "def rocksTiebreaker(tied):\n    includeList1 = []\n    ident = game.playerList\n    idVal = ident\n    includeList1 += idVal\n    excludeList1 = []\n    ident = [x for x in game.playerList if x.name == \"Brooks\"]\n    excludeList1 += ident\n    idList1 = [x for x in includeList1 if x not in excludeList1]\n    voterList = idList1\n    includeList1 = []\n    ident = game.playerList\n    idVal = ident\n    includeList1 += idVal\n    excludeList1 = []\n    ident = [x for x in game.playerList if \"Test\" in x.affiliations]\n    excludeList1 += ident\n    idList1 = [x for x in includeList1 if x not in excludeList1]\n    voteeList = idList1\n    game.directedVote(voterList, voteeList, False)\n    ident = [x for x in game.playerList if \"nominee\" in x.affiliations]\n    del game.voteResults[-1]\n    return ident[0]"
    it "compiles a Tiebreaker with an action into a function name and definition and deletes result if action was an Allocation" $
      runReader (compileTiebreaker (Tiebreak "rocks" (Just (Dec (Allocation "votes" (IdList [IdVal Everyone (Num 1)] [N "Brooks"])))) Nominated)) ids `shouldBe` text "def rocksTiebreaker(tied):\n    includeList1 = []\n    ident = game.playerList\n    idVal = ident\n    includeList1 += idVal\n    excludeList1 = []\n    ident = [x for x in game.playerList if x.name == \"Brooks\"]\n    excludeList1 += ident\n    idList1 = [x for x in includeList1 if x not in excludeList1]\n    game.allocate(idList1, \"votes\")\n    ident = [x for x in game.playerList if \"nominee\" in x.affiliations]\n    del game.allocateResults[-1]\n    return ident[0]"
    it "compiles a Tiebreaker with an action into a function name and definition and deletes result if action was a Competition" $
      runReader (compileTiebreaker (Tiebreak "rocks" (Just (Comp (Scored Individual (IdList [IdVal Everyone (Num 1)] [])))) Nominated)) ids `shouldBe` text "def rocksTiebreaker(tied):\n    includeList1 = []\n    ident = game.playerList\n    idVal = ident\n    includeList1 += idVal\n    excludeList1 = []\n    idList1 = [x for x in includeList1 if x not in excludeList1]\n    game.getScoredCompResults(idList1)\n    ident = [x for x in game.playerList if \"nominee\" in x.affiliations]\n    del game.compResults[-1]\n    return ident[0]"
  describe "compileTiebreakerRef" $ do
    it "compiles a TiebreakerRef into the name of the function that performs the tiebreaker" $
      compileTiebreakerRef (TieRef "Rocks") `shouldBe` text "rocksTiebreaker"
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
      runReader (compileProgression (AU Elimination (IdList [] []))) ids `shouldBe` text "includeList1 = []\nexcludeList1 = []\nidList1 = [x for x in includeList1 if x not in excludeList1]\ngame.eliminate(idList1)"
    it "compiles a CU Progression" $
      runReader (compileProgression (CU (Increase "votes" (Num 1)) (IdList [] []))) ids `shouldBe` text "includeList1 = []\nexcludeList1 = []\nidList1 = [x for x in includeList1 if x not in excludeList1]\nfor player in idList1: player.updateCounter(1, \"votes\")"
  describe "compilePhaseList" $ do
    it "compiles an empty phase list" $ do
      runReader (compilePhaseList []) ids `shouldBe` empty
    it "compiles a non-empty phase list" $ do
      runReader (compilePhaseList [Act (Comp (Scored Individual (IdList [IdVal Everyone (Num 1)] []))), Prog (AU Elimination (IdList [] []))]) ids `shouldBe` text "includeList1 = []\nident = game.playerList\nidVal = ident\nincludeList1 += idVal\nexcludeList1 = []\nidList1 = [x for x in includeList1 if x not in excludeList1]\ngame.getScoredCompResults(idList1)\nincludeList1 = []\nexcludeList1 = []\nidList1 = [x for x in includeList1 if x not in excludeList1]\ngame.eliminate(idList1)"
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
      runReader (compileWinCondition Survive) ids `shouldBe` text "winners = \"\"\nif len(game.playerList) == 1:\n    winners = game.playerList[0].name\nelse:\n    for player in game.playerList[1:]:\n        winners += player.name + \", \"\n    winners += \"and \" + game.playerList[0].name\nprint(winners + \" won!\")"
    it "compiles a Jury Win Condition" $
      runReader (compileWinCondition (Jury 7)) ids `shouldBe` text "game.juryVote(7)\nwinner = getVoteMinOrMax(game.voteResults[-1], True)\nprint(winner.name + \" won!\")"
    it "compiles a FinalComp Team Win Condition" $
      runReader (compileWinCondition (FinalComp Team)) ids `shouldBe` text "game.getTeamCompResults(game.teamList, True, False)\nwinner = game.compResults[-1][\"winner\"]\nprint(winner + \" won!\")"
    it "compiles a FinalComp Individual Win Condition" $
      runReader (compileWinCondition (FinalComp Individual)) ids `shouldBe` text "game.getCompResults(game.playerList, True, False)\nwinner = game.compResults[-1][\"winner\"]\nprint(winner.name + \" won!\")"
    it "compiles a Reach Individual Win Condition" $
      runReader (compileWinCondition (Reach [Gl 4 "points"] Individual)) ids `shouldBe` text "    winners = []\n    for player in game.playerList:\n        if player.checkWinCondition(\"points\", 4):\n            winners.append(player.name)\n    if len(winners) > 0:\n        winnerString = \"\"\n        if len(winners) == 1:\n            winnerString = winners[0]\n        else:\n            for winner in winners[1:]:\n                winnerString += winner + \", \"\n            winnerString += \"and \" + winners[0]\n        print(winnerString + \" won!\")\n        break"
    it "compiles a Reach Team Win Condition" $
      runReader (compileWinCondition (Reach [Gl 4 "points"] Team)) ids `shouldBe` text "    winners = []\n    for player in game.playerList:\n        if player.checkWinCondition(\"points\", 4):\n            winners.append(player)\n    if len(winners) > 0:\n        winTeams = list(dict.fromkeys([x for x in game.teamList for y in winners if x in y.affiliations]))\n        winnerString = \"\"\n        if len(winTeams) == 1:\n            winnerString = winTeams[0]\n        else:\n            for winner in winTeams[1:]:\n                winnerString += winner + \", \"\n            winnerString += \"and \" + winTeams[0]\n        print(winnerString + \" won!\")\n        break"
    it "compiles an Ids Individual Win Condition" $
      runReader (compileWinCondition (Ids (IdList [] []) Individual)) ids `shouldBe` text "includeList1 = []\nexcludeList1 = []\nidList1 = [x for x in includeList1 if x not in excludeList1]\nwinners = \"\"\nif len(idList1) == 0: print(\"No winners!\")\nelif len(idList1) == 1:\n    winners = idList1[0].name\nelse:\n    for winner in idList1[1:]:\n        winners += winner.name + \", \"\n    winners += \"and \" + idList1[0].name\nprint(winners + \" won!\")"
    it "compiles an Ids Team Win Condition" $
      runReader (compileWinCondition (Ids (IdList [] []) Team)) ids `shouldBe` text "includeList1 = []\nexcludeList1 = []\nidList1 = [x for x in includeList1 if x not in excludeList1]\nwinTeams = list(dict.fromkeys([x for x in game.teamList for y in idList1 if x in y.affiliations]))\nwinners = \"\"\nif len(winTeams) == 0: print(\"No winners!\")\nelif len(winTeams) == 1:\n    winners = winTeams[0]\nelse:\n    for winner in winTeams[1:]:\n        winners += winner + \", \"\n    winners += \"and \" + winTeams[0]\nprint(winners + \" won!\")"
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
      getAllCounters (G (PI [P "Brooks" [Affiliation "Great", Counter "votes" (Just 20) (Just 0) Nothing], P "Mac" [Affiliation "Test", Counter "points" Nothing Nothing Nothing]] ["Test", "Great"] False) [] Survive []) `shouldBe` ["votes", "points"]
  describe "compileGame" $ do
    it "compiles a complete Game with no Tiebreakers" $
      compileGame (G (PI [P "Brooks" [Affiliation "Great", Counter "votes" (Just 20) (Just 0) Nothing], P "Mac" [Affiliation "Test", Counter "points" Nothing Nothing Nothing]] ["Test", "Great"] False) [R [Act (Comp (Placed Individual (IdList [IdVal Everyone (Num 1)] []) True False)), Prog (CU (Increase "points" (Num 1)) (IdList [IdVal (Winner (CRef 0)) (Num 1)] []))] 5 []] (Jury 7) []) `shouldBe` text "from gamelib import *\n\ndef roundType1():\n    includeList1 = []\n    ident = game.playerList\n    idVal = ident\n    includeList1 += idVal\n    excludeList1 = []\n    idList1 = [x for x in includeList1 if x not in excludeList1]\n    game.getCompResults(idList1, True, False)\n    includeList1 = []\n    ident = [x for x in game.playerList if x == game.compResults[-1][\"winner\"] or game.compResults[-1][\"winner\"] in x.affiliations]\n    idVal = ident\n    includeList1 += idVal\n    excludeList1 = []\n    idList1 = [x for x in includeList1 if x not in excludeList1]\n    for player in idList1: player.updateCounter(1, \"points\")\n\ngame = Game()\n\ngame.playerList.append(Player(\"Brooks\", [\"Great\"], [{\"counter\": \"votes\", \"starts\": 20, \"min\": 0}]))\ngame.playerList.append(Player(\"Mac\", [\"Test\"], [{\"counter\": \"points\"}]))\ngame.teamList.append(\"Test\")\ngame.teamList.append(\"Great\")\n\nroundList = [roundType1] * 5\n\nfor round in roundList:\n    game.resetResults()\n    round()\n\ngame.juryVote(7)\nwinner = getVoteMinOrMax(game.voteResults[-1], True)\nprint(winner.name + \" won!\")"
    it "compiles a complete Game with a Tiebreaker" $
      compileGame (G (PI [P "Brooks" [Affiliation "Great", Counter "votes" (Just 20) (Just 0) Nothing], P "Mac" [Affiliation "Test", Counter "points" Nothing Nothing Nothing]] ["Test", "Great"] False) [R [Act (Comp (Placed Individual (IdList [IdVal Everyone (Num 1)] []) True False)), Prog (CU (Increase "points" (Num 1)) (IdList [IdVal (Winner (CRef 0)) (Num 1)] []))] 5 []] (Jury 7) [Tiebreak "rocks" Nothing Tied]) `shouldBe` text "from gamelib import *\n\ndef roundType1():\n    includeList1 = []\n    ident = game.playerList\n    idVal = ident\n    includeList1 += idVal\n    excludeList1 = []\n    idList1 = [x for x in includeList1 if x not in excludeList1]\n    game.getCompResults(idList1, True, False)\n    includeList1 = []\n    ident = [x for x in game.playerList if x == game.compResults[-1][\"winner\"] or game.compResults[-1][\"winner\"] in x.affiliations]\n    idVal = ident\n    includeList1 += idVal\n    excludeList1 = []\n    idList1 = [x for x in includeList1 if x not in excludeList1]\n    for player in idList1: player.updateCounter(1, \"points\")\n\ndef rocksTiebreaker(tied):\n    ident = tied\n    return ident[0]\n\ngame = Game()\n\ngame.playerList.append(Player(\"Brooks\", [\"Great\"], [{\"counter\": \"votes\", \"starts\": 20, \"min\": 0}]))\ngame.playerList.append(Player(\"Mac\", [\"Test\"], [{\"counter\": \"points\"}]))\ngame.teamList.append(\"Test\")\ngame.teamList.append(\"Great\")\n\nroundList = [roundType1] * 5\n\nfor round in roundList:\n    game.resetResults()\n    round()\n\ngame.juryVote(7)\nwinner = getVoteMinOrMax(game.voteResults[-1], True)\nprint(winner.name + \" won!\")"