from gamelib import *

def reVoteTiebreaker(tied):
    voters = [x for x in game.playerList if game.compResults[0]["loser"] in x.affiliations and x not in tied]
    game.vote(voters, tied)
    tiebroken = getVoteMinOrMax(game.voteResults[1], True, pastVotesTiebreaker)
    del game.voteResults[1] #Tie vote results should not be saved in round vote history
    return tiebroken

def pastVotesTiebreaker(tied):
    tiebroken = getMinOrMax(tied, "votesAgainst", True, rocksTiebreaker)
    return tiebroken

def rocksTiebreaker(tied):
    atRisk = [x for x in game.playerList if game.compResults[0]["loser"] in x.affiliations and x not in tied]
    badRock = random.randint(0, len(atRisk)-1)
    return atRisk[badRock]

def reVoteTiebreaker2(tied):
    voters = [x for x in game.playerList if x not in tied]
    game.vote(voters, tied)
    tiebroken = getVoteMinOrMax(game.voteResults[1], True, pastVotesTiebreaker2)
    del game.voteResults[1] #Tie vote results should not be saved in round vote history
    return tiebroken

def pastVotesTiebreaker2(tied):
    tiebroken = getMinOrMax(tied, "votesAgainst", True, rocksTiebreaker2)
    return tiebroken

def rocksTiebreaker2(tied):
    atRisk = [x for x in game.playerList if x != game.compResults[0]["winner"] and x not in tied]
    badRock = random.randint(0, len(atRisk)-1)
    return atRisk[badRock]

def roundType1():
    game.getTeamCompResults(game.teamList, False, True)
    loser = game.compResults[0]["loser"]
    game.vote([x for x in game.playerList if loser in x.affiliations], [x for x in game.playerList if loser in x.affiliations])
    game.eliminate(getVoteMinOrMax(game.voteResults[0], True, reVoteTiebreaker))
    for player in [x for x in game.playerList if loser in x.affiliations]:
        player.updateCounter(game.voteResults[0][player], "votesAgainst")

def roundType2():
    swap(["Kucha", "Ogakor"], [], game.playerList, False)
    roundType1()

def roundType3():
    game.getCompResults(game.playerList, True, False)
    game.vote(game.playerList, [x for x in game.playerList if x != game.compResults[0]["winner"]])
    game.eliminate(getVoteMinOrMax(game.voteResults[0], True, reVoteTiebreaker2))
    for player in game.playerList:
        player.updateCounter(game.voteResults[0][player], "votesAgainst")

def roundType4():
    merge(["Kucha", "Ogakor"], "Merged", game.playerList)
    roundType3()

game = Game()

game.playerList.append(Player("Debb", [], [{"counter": "votesAgainst"}]))
game.playerList.append(Player("Kel", [], [{"counter": "votesAgainst"}]))
game.playerList.append(Player("Maralyn", [], [{"counter": "votesAgainst"}]))
game.playerList.append(Player("Mitchell", [], [{"counter": "votesAgainst"}]))
game.playerList.append(Player("Kimmi", [], [{"counter": "votesAgainst"}]))
game.playerList.append(Player("Michael", [], [{"counter": "votesAgainst"}]))
game.playerList.append(Player("Jeff", [], [{"counter": "votesAgainst"}]))
game.playerList.append(Player("Alicia", [], [{"counter": "votesAgainst"}]))
game.playerList.append(Player("Jerri", [], [{"counter": "votesAgainst"}]))
game.playerList.append(Player("Nick", [], [{"counter": "votesAgainst"}]))
game.playerList.append(Player("Amber", [], [{"counter": "votesAgainst"}]))
game.playerList.append(Player("Rodger", [], [{"counter": "votesAgainst"}]))
game.playerList.append(Player("Elizabeth", [], [{"counter": "votesAgainst"}]))
game.playerList.append(Player("Keith", [], [{"counter": "votesAgainst"}]))
game.playerList.append(Player("Colby", [], [{"counter": "votesAgainst"}]))
game.playerList.append(Player("Tina", [], [{"counter": "votesAgainst"}]))
game.teamList.append("Kucha")
game.teamList.append("Ogakor")
randomlyDivideTeams(game.teamList, game.playerList)

roundList = [roundType1] * 4 + [roundType2] + [roundType1] + [roundType4] + [roundType3] * 7

for round in roundList:
    game.resetResults()
    round()

game.juryVote(7)

winner = getVoteMinOrMax(game.voteResults[-1], True)
print(winner.name + " won!")
