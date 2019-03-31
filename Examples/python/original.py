from gamelib import *

def lowVoteTiebreaker(tied):
    return getMinOrMax(tied, "votes", False, randomTiebreaker)

def randomTiebreaker(tied):
    return tied[random.randint(0, len(tied)-1)]

def roundType1():
    game.getCompResults(game.playerList, True, False)
    game.compResults[0]["winner"].updateCounter(3, "votes")
    game.allocate(game.playerList, "votes")
    directedVoters = []
    for allocation in game.allocateResults[0]:
        directedVoters += [allocation["player"]] * allocation["allocated"]
    game.directedVote(directedVoters, game.playerList, True)
    for playerResult in game.voteResults[0]:
        playerResult["player"].updateCounter(playerResult["votes"], "position")
    game.eliminate(getMinOrMax(game.playerList, "position", False, lowVoteTiebreaker))

game = Game()

game.playerList.append(Player("Anna", [], [{"counter": "votes", "starts": 20, "min": 0}, {"counter": "position"}]))
game.playerList.append(Player("Bob", [], [{"counter": "votes", "starts": 20, "min": 0}, {"counter": "position"}]))
game.playerList.append(Player("Carrie", [], [{"counter": "votes", "starts": 20, "min": 0}, {"counter": "position"}]))
game.playerList.append(Player("Dennis", [], [{"counter": "votes", "starts": 20, "min": 0}, {"counter": "position"}]))
game.playerList.append(Player("Emma", [], [{"counter": "votes", "starts": 20, "min": 0}, {"counter": "position"}]))
game.playerList.append(Player("Frank", [], [{"counter": "votes", "starts": 20, "min": 0}, {"counter": "position"}]))
game.playerList.append(Player("Grace", [], [{"counter": "votes", "starts": 20, "min": 0}, {"counter": "position"}]))
game.playerList.append(Player("Harold", [], [{"counter": "votes", "starts": 20, "min": 0}, {"counter": "position"}]))
game.playerList.append(Player("Ivy", [], [{"counter": "votes", "starts": 20, "min": 0}, {"counter": "position"}]))
game.playerList.append(Player("Jeff", [], [{"counter": "votes", "starts": 20, "min": 0}, {"counter": "position"}]))

roundList = [roundType1] * 9

for round in roundList:
    game.resetResults()
    round()

winners = ""
if len(game.playerList) == 1:
    winners = game.playerList[0].name
else:
    for player in game.playerList[1:]:
        winners += player.name + ", "
    winners += "and " + game.playerList[0].name

print(winners + " won!")