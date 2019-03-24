class Player:

    def __init__(self, name):
        self.name = name
        self.set = 0

def getCompResults(compPlayers):
    print("Competition between:")
    for player in compPlayers:
        print(player.name)
    haveWinner = False
    playerNames = [x.name for x in compPlayers]
    while not haveWinner:
        winName = input("Who won this round?\n")
        if winName in playerNames:
            haveWinner = True
        else:
            print(winName + " is not an eligible winner!")
    compDict = {}
    for player in compPlayers:
            if player.name == winName:
                winner = player
                compDict.update({"winner": winner})
    compResults.append(compDict)

def checkWinCondition():
    for player in playerList:
        if player.set >= 4:
            return True
    return False

def roundType1():
    getCompResults(playerList)
    compResults[0]["winner"].set = compResults[0]["winner"].set + 1

john = Player("John")
joe = Player("Joe")
playerList = [john, joe]

roundList = [roundType1] * 7

for round in roundList:
    compResults = []
    round()
    if checkWinCondition():
        print(compResults[0]["winner"].name + " won!") 
        break
