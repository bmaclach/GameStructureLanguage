from gamelib import *

def roundType1():
    game.getCompResults(game.playerList, True, False)
    game.compResults[-1]["winner"].updateCounter(1, "sets")

game = Game()

game.playerList.append(Player("John", [], [{"counter": "sets", "starts": 0}]))
game.playerList.append(Player("Joe", [], [{"counter": "sets", "starts": 0}]))

roundList = [roundType1] * 7

for round in roundList:
    game.resetResults()
    round()
    if game.checkWinCondition("sets", 4):
        print(game.compResults[0]["winner"].name + " won!") 
        break
