class Player:

    def __init__(self, name):
        self.name = name
        self.set = 0

def getCompResults(compPlayers):
    print("Competition between:")
    for player in compPlayers:
        print(player.name)
    winName = input("Who won this round?\n")
    if len(compPlayers) > 2:
        loserDetermined = False
        while not loserDetermined:
            loseName = input("Who lost this round?\n")
            if loseName != winName:
                loserDetermined = True
            else:
                print("Loser cannot be same as winner!\n")
        for player in compPlayers:
            if player.name == winName:
                winner = player
            elif player.name == loseName:
                loser = player
    else:
        for player in compPlayers:
            if player.name == winName:
                winner = player
            else:
                loser = player
    return [winner, loser]

def checkWinCondition():
    for player in playerList:
        if player.set >= 4:
            return True
    return False

john = Player("John")
joe = Player("Joe")
playerList = [john, joe]

for i in range(7):
    roundWin = getCompResults(playerList)[0]
    roundWin.set = roundWin.set + 1
    if checkWinCondition():
        print(roundWin.name + " won!") 
        break
