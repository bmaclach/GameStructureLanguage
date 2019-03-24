class Player:

    def __init__(self, name):
        self.name = name
        self.counters = {"set": 0}

    def updateCounter(self, change, counter):
        self.counters[counter] += change
        return change

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
        if player.counters["set"] >= 4:
            return True
    return False

def roundType1():
    getCompResults(playerList)
    compResults[0]["winner"].updateCounter(1, "set")

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
