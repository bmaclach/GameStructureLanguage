from gamelib import *

def roundType1():
    game.getCompResults(game.playerList, True, True)
    game.vote([game.compResults[-1]["winner"]], [x for x in game.playerList if x != game.compResults[0]["loser"]])
    game.vote([game.compResults[-1]["loser"]], [x for x in game.playerList if x != game.compResults[0]["winner"] and x != getVoteMinOrMax(game.voteResults[0], True)])
    game.getCompResults([game.compResults[-1]["loser"], getVoteMinOrMax(game.voteResults[-1], True)], False, True)
    game.eliminate([game.compResults[-1]["loser"]])

def roundType2():
    game.getCompResults(game.playerList, True, False)
    game.getCompResults([x for x in game.playerList if x != game.compResults[-1]["winner"]], False, True)
    game.eliminate([game.compResults[-1]["loser"]])

game = Game()

game.playerList.append(Player("Junseok"))
game.playerList.append(Player("Minseo"))
game.playerList.append(Player("Minsoo"))
game.playerList.append(Player("Changyeop"))
game.playerList.append(Player("Jungmoon"))
game.playerList.append(Player("Gura"))
game.playerList.append(Player("Poong"))
game.playerList.append(Player("Yuram"))
game.playerList.append(Player("Eunji"))
game.playerList.append(Player("Sunggyu"))
game.playerList.append(Player("Sangmin"))
game.playerList.append(Player("Kyungran"))
game.playerList.append(Player("Jinho"))

roundList = [roundType1] * 10 + [roundType2]

for round in roundList:
    game.resetResults()
    round()

game.getCompResults(game.playerList, True, False)
print(game.compResults[-1]["winner"].name + " won!")
