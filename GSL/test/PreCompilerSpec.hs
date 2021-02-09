module PreCompilerSpec (spec) where

import Test.Hspec

import AST
import PreCompiler

import Control.Monad.Reader (runReader)

exPhase1, exPhase2, exPhase3, exPhase4 :: Phase
exPhase1 = Act (Comp (Scored Team (IdList [IdVal Everyone (Num 1)] [])))
exPhase2 = Prog (CU (Increase "votes" (Num 3)) (IdList [IdVal (N "Brooks") (Num 1)] []))
exPhase3 = Prog (AU Elimination (IdList [IdVal (N "Test") (Num 1)] []))
exPhase4 = Act (Dec (Allocation "votes" (IdList [IdVal Everyone (Num 1)] [])))

exGame :: Game
exGame = G (PI [P "Brooks" [], P "Test" []] [] False) [R [exPhase1, exPhase2] 10 [From 8 Before 1 exPhase3], R [exPhase4, exPhase1] 7 [Jst 4 During 2 exPhase3, From 5 After 1 exPhase2]] Survive []

spec :: Spec
spec = do
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
      addPhaseToRound exPhase4 During 2 (R [exPhase1, exPhase2, exPhase3] 1 [Jst 3 During 2 exPhase4, From 5 During 1 exPhase4]) `shouldBe` R [exPhase1, exPhase4, exPhase3] 1 [From 5 During 1 exPhase4]
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
      applyModifiers exGame `shouldBe` G (PI [P "Brooks" [], P "Test" []] [] False) [R [exPhase1, exPhase2] 7 [], R [exPhase3, exPhase1, exPhase2] 3 [], R [exPhase4, exPhase1] 3 [], R [exPhase4, exPhase3] 1 [], R [exPhase4, exPhase2, exPhase1] 3 []] Survive []
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
    let runGetCompRefId id n b = runReader (getCompRefId id n b)
    it "adds to CompInfo if winner encountered" $
      runGetCompRefId (Winner (CRef 2)) 3 False [] `shouldBe` [(2, True, False)]
    it "adds to CompInfo if winner encountered with 0 index" $
      runGetCompRefId (Winner (CRef 0)) 3 False [] `shouldBe` [(3, True, False)]
    it "adds to CompInfo if loser encountered" $
      runGetCompRefId (Loser (CRef 2)) 3 False [] `shouldBe` [(2, False, True)]
    it "adds to CompInfo if loser encountered with 0 index" $
      runGetCompRefId (Loser (CRef 0)) 3 False [] `shouldBe` [(3, False, True)]
    it "adds to CompInfo if Chance encountered" $
      runGetCompRefId (Chance 1 (IdList [IdVal (Winner (CRef 2)) (Num 1)] [])) 3 False [] 
              `shouldBe` [(2, True, False)]
    it "adds to CompInfo if Majority encountered with tiebreaker" $
      runGetCompRefId (Majority (VRef 1) (Just (TieRef "a"))) 3 False [Tiebreak "a" Nothing (Winner (CRef 2))] `shouldBe` [(2, True, False)]
    it "does not add to CompInfo if Majority with tiebreaker with index 0 is encountered" $
      runGetCompRefId (Majority (VRef 1) (Just (TieRef "a"))) 3 False [Tiebreak "a" Nothing (Winner (CRef 0))] `shouldBe` []
    it "adds to CompInfo if Minority encountered with tiebreaker" $
      runGetCompRefId (Minority (VRef 1) (Just (TieRef "a"))) 3 False [Tiebreak "a" Nothing (Winner (CRef 2))] `shouldBe` [(2, True, False)]
    it "does not add to CompInfo if Minority with tiebreaker with index 0 is encountered" $
      runGetCompRefId (Minority (VRef 1) (Just (TieRef "a"))) 3 False [Tiebreak "a" Nothing (Winner (CRef 0))] `shouldBe` []
    it "adds to CompInfo if Most with no tiebreaker encountered" $
      runGetCompRefId (Most "votes" (IdList [IdVal (Winner (CRef 2)) (Num 1)] []) Nothing) 3 False [] `shouldBe` [(2, True, False)]
    it "adds to CompInfo if Most with tiebreaker encountered" $
      runGetCompRefId (Most "votes" (IdList [IdVal (Winner (CRef 2)) (Num 1)] []) (Just (TieRef "a"))) 3 False [Tiebreak "a" Nothing (Winner (CRef 1))] `shouldBe` [(2, True, False), (1, True, False)]
    it "does not add tiebreaker to CompInfo if Most with tiebreaker with index 0 is encountered" $
      runGetCompRefId (Most "votes" (IdList [IdVal (Winner (CRef 2)) (Num 1)] []) (Just (TieRef "a"))) 3 False [Tiebreak "a" Nothing (Winner (CRef 0))] `shouldBe` [(2, True, False)]
    it "adds to CompInfo if Least with no tiebreaker encountered" $
      runGetCompRefId (Least "votes" (IdList [IdVal (Winner (CRef 2)) (Num 1)] []) Nothing) 3 False [] `shouldBe` [(2, True, False)]
    it "adds to CompInfo if Least with tiebreaker encountered" $
      runGetCompRefId (Least "votes" (IdList [IdVal (Winner (CRef 2)) (Num 1)] []) (Just (TieRef "a"))) 3 False [Tiebreak "a" Nothing (Winner (CRef 1))] `shouldBe` [(2, True, False), (1, True, False)]
    it "does not add tiebreaker to CompInfo if Least with tiebreaker with index 0 is encountered" $
      runGetCompRefId (Least "votes" (IdList [IdVal (Winner (CRef 2)) (Num 1)] []) (Just (TieRef "a"))) 3 False [Tiebreak "a" Nothing (Winner (CRef 0))] `shouldBe` [(2, True, False)]
  describe "getCompRefIdentifiers" $
    it "adds info from list of identifiers to CompInfo" $
      runReader (getCompRefIdentifiers [Winner (CRef 2), Everyone, Loser (CRef 2), N "Brooks"] 3 False) [] `shouldBe` [(2, True, True)]
  describe "getCompRefIdVals" $
    it "adds info from list of IdentifierVals to CompInfo" $
      runReader (getCompRefIdVals [IdVal (Winner (CRef 2)) (Num 1), IdVal Everyone (Num 1), IdVal (Loser (CRef 2)) (Num 1), IdVal (N "Brooks") (Num 1)] 3 False) [] `shouldBe` [(2, True, True)]
  describe "getCompRefIdList" $
    it "adds info from IdentifierList to CompInfo" $
      runReader (getCompRefIdList (IdList [IdVal (Winner (CRef 2)) (Num 1), IdVal Everyone (Num 1), IdVal (Loser (CRef 2)) (Num 1), IdVal (N "Brooks") (Num 1)] [Winner (CRef 1), Everyone, Loser (CRef 1), N "Brooks"]) 3 False) [] `shouldBe` [(2, True, True), (1, True, True)]
  describe "getCompRefs" $ do
    it "extracts CompInfo from scored comp" $
      runReader (getCompRefs [Act (Comp (Scored Team (IdList [IdVal Everyone (Num 1)] [Winner (CRef 1)])))] 2) [] `shouldBe` [(1, True, False)]
    it "extracts CompInfo from placed comp" $    
      runReader (getCompRefs [Act (Comp (Placed Team (IdList [IdVal Everyone (Num 1)] [Winner (CRef 1)]) True True))] 2) [] `shouldBe` [(1, True, False)]
    it "extracts CompInfo from Vote" $
      runReader (getCompRefs [Act (Dec (Vote (IdList [IdVal Everyone (Num 1)] [Loser (CRef 1)]) (IdList [IdVal Everyone (Num 1)] [Winner (CRef 1)]) False))] 2) [] `shouldBe` [(1, True, True)]
    it "extracts CompInfo from Nomination" $
      runReader (getCompRefs [Act (Dec (Nomination 2 (IdList [IdVal Everyone (Num 1)] [Loser (CRef 1)]) (IdList [IdVal Everyone (Num 1)] [Winner (CRef 1)]) False))] 2) [] `shouldBe` [(1, True, True)]
    it "extracts CompInfo from Allocation" $
      runReader (getCompRefs [Act (Dec (Allocation "votes" (IdList [IdVal Everyone (Num 1)] [Loser (CRef 1)])))] 2) [] `shouldBe` [(1, False, True)]
    it "extracts CompInfo from DirectedVote" $
      runReader (getCompRefs [Act (Dec (DirectedVote (IdList [IdVal Everyone (Num 1)] [Loser (CRef 1)]) (IdList [IdVal Everyone (Num 1)] [Winner (CRef 1)]) False))] 2) [] `shouldBe` [(1, True, True)]
    it "extracts CompInfo from Uses" $
      runReader (getCompRefs [Act (Dec (Uses (Winner (CRef 1)) [] []))] 2) [] `shouldBe` [(1, True, False)]
    it "extracts CompInfo from AffiliationUpdate" $
      runReader (getCompRefs [Prog (AU Elimination (IdList [IdVal Everyone (Num 1)] [Loser (CRef 1)]))] 2) [] `shouldBe` [(1, False, True)]
    it "extracts CompInfo from CounterUpdate" $
      runReader (getCompRefs [Prog (CU (Increase "votes" (Num 3)) (IdList [IdVal Everyone (Num 1)] [Loser (CRef 1)]))] 2) [] `shouldBe` [(1, False, True)]
    it "increases competition counter when scored and placed competitions are encountered" $
      runReader (getCompRefs [Act (Comp (Scored Team (IdList [IdVal Everyone (Num 1)] [Winner (CRef 1)]))), Act (Comp (Placed Team (IdList [IdVal Everyone (Num 1)] [Winner (CRef 0)]) True True)), Act (Dec (Allocation "votes" (IdList [IdVal Everyone (Num 1)] [Loser (CRef 0)])))] 1) [] `shouldBe` [(1, True, False), (2, True, False), (3, False, True)]
  describe "updateComp" $ do
    it "does not change Scored competition" $
      updateComp (Scored Team (IdList [IdVal Everyone (Num 1)] [])) 1 [(1, True, True)] `shouldBe` Scored Team (IdList [IdVal Everyone (Num 1)] [])
    it "updates Placed competition with info from CompInfo" $
      updateComp (Placed Team (IdList [IdVal Everyone (Num 1)] []) True True) 1 [(2, False, False), (1, False, True)] `shouldBe` Placed Team (IdList [IdVal Everyone (Num 1)] []) False True
    it "updates Placed competition with False if it is not in CompInfo" $
      updateComp (Placed Team (IdList [IdVal Everyone (Num 1)] []) True True) 3 [(2, False, False), (1, False, True)] `shouldBe` Placed Team (IdList [IdVal Everyone (Num 1)] []) False False
  describe "updatePhaseComps" $ do
    it "updates all Placed competitions in a list of phases" $
      updatePhaseComps [Act (Comp (Placed Team (IdList [IdVal Everyone (Num 1)] []) True True)), Act (Comp (Scored Team (IdList [IdVal Everyone (Num 1)] []))), Act (Comp (Placed Team (IdList [IdVal Everyone (Num 1)] []) True True))] 1 [(1, False, True)] `shouldBe` [Act (Comp (Placed Team (IdList [IdVal Everyone (Num 1)] []) False True)), Act (Comp (Scored Team (IdList [IdVal Everyone (Num 1)] []))), Act (Comp (Placed Team (IdList [IdVal Everyone (Num 1)] []) False False))]
  describe "updateRoundComps" $ do
    it "updates all Placed competitions in a round" $
      runReader (updateRoundComps (R [Act (Comp (Placed Team (IdList [IdVal Everyone (Num 1)] []) True True)), Act (Comp (Scored Team (IdList [IdVal (Winner (CRef 0)) (Num 1)] []))), Act (Comp (Placed Team (IdList [IdVal (Loser (CRef 0)) (Num 1)] []) True True))] 5 [])) [] `shouldBe` R [Act (Comp (Placed Team (IdList [IdVal Everyone (Num 1)] []) True False)), Act (Comp (Scored Team (IdList [IdVal (Winner (CRef 0)) (Num 1)] []))), Act (Comp (Placed Team (IdList [IdVal (Loser (CRef 0)) (Num 1)] []) False False))] 5 []
  describe "updateComps" $ do
    it "updates all Placed competitions in a Game" $
      updateComps (G (PI [] [] False) [R [Act (Comp (Placed Team (IdList [IdVal Everyone (Num 1)] []) True True)), Act (Comp (Scored Team (IdList [IdVal (Winner (CRef 0)) (Num 1)] []))), Act (Comp (Placed Team (IdList [IdVal (Loser (CRef 0)) (Num 1)] []) True True))] 5 [], R [Act (Comp (Placed Team (IdList [IdVal Everyone (Num 1)] []) True True)), Act (Comp (Scored Team (IdList [IdVal (Winner (CRef 1)) (Num 1)] []))), Act (Comp (Placed Team (IdList [IdVal (Loser (CRef 1)) (Num 1)] []) True True))] 3 []] Survive [Tiebreak "fire" (Just (Comp (Placed Individual (IdList [IdVal Everyone (Num 1)] []) False False))) (Loser (CRef 0))]) `shouldBe` G (PI [] [] False) [R [Act (Comp (Placed Team (IdList [IdVal Everyone (Num 1)] []) True False)), Act (Comp (Scored Team (IdList [IdVal (Winner (CRef 0)) (Num 1)] []))), Act (Comp (Placed Team (IdList [IdVal (Loser (CRef 0)) (Num 1)] []) False False))] 5 [], R [Act (Comp (Placed Team (IdList [IdVal Everyone (Num 1)] []) True True)), Act (Comp (Scored Team (IdList [IdVal (Winner (CRef 1)) (Num 1)] []))), Act (Comp (Placed Team (IdList [IdVal (Loser (CRef 1)) (Num 1)] []) False False))] 3 []] Survive [Tiebreak "fire" (Just (Comp (Placed Individual (IdList [IdVal Everyone (Num 1)] []) False True))) (Loser (CRef 0))]
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
      getAllNames (G (PI [P "Brooks" [], P "Test" []] [] False) [] Survive []) `shouldBe` ["Brooks", "Test"]
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
  describe "getAffsFromAction" $ do
    it "returns affiliations from the 'then' branch of a Uses" $
      getAffsFromAction (Dec (Uses (N "Brooks") [Prog (AU (Add "Jays") (IdList [] []))] []))  []`shouldBe` ["Jays"]
    it "returns affiliations from the 'otherwise' branch of a Uses" $
      getAffsFromAction (Dec (Uses (N "Brooks") [] [Prog (AU (Add "Jays") (IdList [] []))])) [] `shouldBe` ["Jays"]
  describe "getAffsFromTiebreaker" $ do
    it "returns affiliations from a tiebreaker with a uses action" $
      getAffsFromTiebreaker (Tiebreak "usestie" (Just (Dec (Uses (N "Brooks") [] [Prog (AU (Add "Jays") (IdList [] []))]))) (N "Brooks")) [] `shouldBe` ["Jays"]
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
      getAllAffiliations (G (PI [P "Brooks" [Affiliation "Jays"], P "Test" [Affiliation "Yankees"]] ["Jays", "Yankees"] False) [R [Prog (AU (Add "Kucha") (IdList [] [])), Prog (AU (Add "Jays") (IdList [] []))] 5 []] Survive [Tiebreak "usestie" (Just (Dec (Uses (N "Brooks") [] [Prog (AU (Add "Ogakor") (IdList [] []))]))) (N "Brooks")]) [] `shouldBe` ["nominee", "Jays", "Yankees", "Kucha", "Ogakor"]
  describe "updateTiebreakIds" $ do
    it "changes N to A in tiebreaker with no action" $
      updateTiebreakIds [] ["Test"] (Tiebreak "a" Nothing (N "Test")) `shouldBe` Tiebreak "a" Nothing (A "Test")
    it "changes N to A in tiebreaker with action" $
      updateTiebreakIds [] ["Test"] (Tiebreak "a" (Just (Comp (Scored Team (IdList [] [N "Test"])))) (N "Test")) `shouldBe` Tiebreak "a" (Just (Comp (Scored Team (IdList [] [A "Test"])))) (A "Test")
  describe "updateId" $ do
    it "changes an N to an A if it is an affiliation" $
      updateId [] ["Test"] (N "Test") `shouldBe` A "Test"
    it "does not change an N if it is a player name" $
      updateId ["Test"] [] (N "Test") `shouldBe` N "Test"
    it "changes an N in a Chance identifier" $
      updateId [] ["Test"] (Chance 1 (IdList [] [N "Test"])) `shouldBe`Chance 1 (IdList [] [A "Test"])
    it "changes an N in a Most" $
      updateId [] ["Test"] (Most "votes" (IdList [] [N "Test"]) Nothing) `shouldBe` Most "votes" (IdList [] [A "Test"]) Nothing
    it "changes an N in a Least" $
      updateId [] ["Test"] (Least "votes" (IdList [] [N "Test"]) Nothing) `shouldBe` Least "votes" (IdList [] [A "Test"]) Nothing
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
      updateIdListIds ["Brooks"] ["Test"] (IdList [IdVal (N "Test") (Num 1), IdVal (N "Brooks") (Num 1)] [N "Brooks", N "Test"]) `shouldBe` IdList [IdVal (A "Test") (Num 1), IdVal (N "Brooks") (Num 1)] [N "Brooks", A "Test"]
  describe "updateActionIds" $ do
    it "changes N to A in a Scored competition" $
      updateActionIds [] ["Test"] (Comp (Scored Team (IdList [] [N "Test"]))) `shouldBe` Comp (Scored Team (IdList [] [A "Test"]))
    it "changes N to A in a Placed competition" $
      updateActionIds [] ["Test"] (Comp (Placed Team (IdList [] [N "Test"]) False False)) `shouldBe` Comp (Placed Team (IdList [] [A "Test"]) False False)
    it "changes N to A in a Vote" $
      updateActionIds [] ["Test"] (Dec (Vote (IdList [] [N "Test"]) (IdList [] [N "Test"]) False)) `shouldBe` Dec (Vote (IdList [] [A "Test"]) (IdList [] [A "Test"]) False)
    it "changes N to A in a Nomination" $
      updateActionIds [] ["Test"] (Dec (Nomination 2 (IdList [] [N "Test"]) (IdList [] [N "Test"]) False)) `shouldBe` Dec (Nomination 2 (IdList [] [A "Test"]) (IdList [] [A "Test"]) False)
    it "changes N to A in an Allocation" $
      updateActionIds [] ["Test"] (Dec (Allocation "votes" (IdList [] [N "Test"]))) `shouldBe` Dec (Allocation "votes" (IdList [] [A "Test"]))
    it "changes N to A in a DirectedVote" $
      updateActionIds [] ["Test"] (Dec (DirectedVote (IdList [] [N "Test"]) (IdList [] [N "Test"]) False)) `shouldBe` Dec (DirectedVote (IdList [] [A "Test"]) (IdList [] [A "Test"]) False)
    it "changes N to A in a Uses" $
      updateActionIds [] ["Test"] (Dec (Uses (N "Test") [Act (Comp (Scored Team (IdList [] [N "Test"])))] [Act (Comp (Scored Team (IdList [] [N "Test"])))])) `shouldBe` Dec (Uses (A "Test") [Act (Comp (Scored Team (IdList [] [A "Test"])))] [Act (Comp (Scored Team (IdList [] [A "Test"])))])
  describe "updatePhaseIds" $ do
    it "changes N to A in an Action" $
      updatePhaseIds [] ["Test"] [Act (Comp (Scored Team (IdList [] [N "Test"])))] `shouldBe` [Act (Comp (Scored Team (IdList [] [A "Test"])))]
    it "changes N to A in an AffiliationUpdate" $
      updatePhaseIds [] ["Test"] [Prog (AU Elimination (IdList [] [N "Test"]))] `shouldBe` [Prog (AU Elimination (IdList [] [A "Test"]))]
    it "changes N to A in a CounterUpdate" $
      updatePhaseIds [] ["Test"] [Prog (CU (Set "votes" (Num 0)) (IdList [] [N "Test"]))] `shouldBe` [Prog (CU (Set "votes" (Num 0)) (IdList [] [A "Test"]))]
    it "changes N to A in multiple phases" $
      updatePhaseIds [] ["Test"] [Prog (CU (Set "votes" (Num 0)) (IdList [] [N "Test"])), Prog (AU Elimination (IdList [] [N "Test"]))] `shouldBe` [Prog (CU (Set "votes" (Num 0)) (IdList [] [A "Test"])), Prog (AU Elimination (IdList [] [A "Test"]))]
  describe "updateRoundIds" $ do
    it "changes N to A in a Round" $
      updateRoundIds [] ["Test"] (R [Prog (CU (Set "votes" (Num 0)) (IdList [] [N "Test"])), Prog (AU Elimination (IdList [] [N "Test"]))] 5 []) `shouldBe` R [Prog (CU (Set "votes" (Num 0)) (IdList [] [A "Test"])), Prog (AU Elimination (IdList [] [A "Test"]))] 5 []
  describe "updateIds" $ do
    it "changes N to A in a Game" $
      updateIds (G (PI [P "Brooks" [Affiliation "Test"]] [] False) [R [Prog (CU (Set "votes" (Num 0)) (IdList [] [N "Test"])), Prog (AU Elimination (IdList [] [N "Test"]))] 5 [], R [Act (Comp (Scored Team (IdList [] [N "Test"])))] 3 []] Survive [Tiebreak "a" Nothing (N "Test")]) `shouldBe` G (PI [P "Brooks" [Affiliation "Test"]] [] False) [R [Prog (CU (Set "votes" (Num 0)) (IdList [] [A "Test"])), Prog (AU Elimination (IdList [] [A "Test"]))] 5 [], R [Act (Comp (Scored Team (IdList [] [A "Test"])))] 3 []] Survive [Tiebreak "a" Nothing (A "Test")]
  describe "preCompile" $ do
    it "applies modifiers, updates competitions, and updates ids" $
      preCompile (G (PI [P "Brooks" [Affiliation "Test"]] [] False) [R [Prog (CU (Set "votes" (Num 0)) (IdList [] [N "Test"])), Prog (AU Elimination (IdList [] [N "Test"]))] 5 [From 4 Before 2 exPhase1], R [Act (Comp (Placed Team (IdList [] [N "Test"]) True True)), Prog (AU Elimination (IdList [] [Loser (CRef 0)]))] 3 []] Survive [Tiebreak "fire" (Just (Comp (Placed Individual (IdList [IdVal (N "Test") (Num 0)] []) False False))) (Winner (CRef 0))]) `shouldBe` G (PI [P "Brooks" [Affiliation "Test"]] [] False) [R [Prog (CU (Set "votes" (Num 0)) (IdList [] [A "Test"])), Prog (AU Elimination (IdList [] [A "Test"]))] 3 [], R [Prog (CU (Set "votes" (Num 0)) (IdList [] [A "Test"])), exPhase1, Prog (AU Elimination (IdList [] [A "Test"]))] 2 [], R [Act (Comp (Placed Team (IdList [] [A "Test"]) False True)), Prog (AU Elimination (IdList [] [Loser (CRef 0)]))] 3 []] Survive [Tiebreak "fire" (Just (Comp (Placed Individual (IdList [IdVal (A "Test") (Num 0)] []) True False))) (Winner (CRef 0))]