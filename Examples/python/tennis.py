from gamelib import *

def roundType1():
    game.getCompResults(game.playerList, True, False)
    game.compResults[-1]["winner"].updateCounter(1, "sets")

game = Game()

game.playerList.append(Player("John", [], [{"counter": "sets"}]))
game.playerList.append(Player("Joe", [], [{"counter": "sets"}]))

roundList = [roundType1] * 7

for round in roundList:
    game.resetResults()
    round()
    for player in game.playerList:
        if player.checkWinCondition("sets", 4):
            print(game.compResults[0]["winner"].name + " won!") 
            break
