from gamelib import *

def roundType1():
    game.getScoredTeamCompResults(game.teamList)
    for teamScore in game.compResults[0]["scores"]:
        teamPlayers = [x for x in game.playerList if teamScore["team"] in x.affiliations]
        for player in teamPlayers:
            player.updateCounter(teamScore["score"], "points")

game = Game()

game.playerList.append(Player("Jose", ["Jays"], [{"counter": "points"}]))
game.playerList.append(Player("Kevin", ["Jays"], [{"counter": "points"}]))
game.playerList.append(Player("John", ["Yankees"], [{"counter": "points"}]))
game.playerList.append(Player("Joe", ["Yankees"], [{"counter": "points"}]))
game.teamList.append("Jays")
game.teamList.append("Yankees")

roundList = [roundType1] * 9

for round in roundList:
    game.resetResults()
    round()
    
winner = getMinOrMax(game.playerList, "points", True)
for team in game.teamList:
    if team in winner.affiliations:
        print(team + " won!")
        break
        