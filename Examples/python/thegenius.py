class Player:

    def __init__(self, name):
        self.name = name

def defaultTiebreaker(tied):
    return tied[0]

def getCompResults(compPlayers):
    print("Competition between:")
    for player in compPlayers:
        print(player.name)
    haveWinner = False
    playerNames = [x.name for x in compPlayers]
    compDict = {}
    while not haveWinner:
        winName = input("Who won this round?\n")
        if winName in playerNames:
            haveWinner = True
        else:
            print(winName + " is not playing in this competition!")
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
    compDict.update({"winner": winner,
                     "loser": loser})
    compResults.append(compDict)

def vote(voters, votees, selfVote=False):
    print("Vote between:")
    for votee in votees:
        print(votee.name)
    voteeNames = [x.name for x in votees]
    votes = []
    voteDict = []
    for voter in voters:
        haveVote = False
        while not haveVote:
            playerVote = input("Who does " + voter.name + " vote for?\n")
            if not selfVote and playerVote == voter.name:
                print("You cannot vote for yourself!")
            elif playerVote in voteeNames:
                votes.append(playerVote)
                haveVote = True
            else:
                print(playerVote + " is not eligible to be voted for!")
    for votee in votees:
        n = votes.count(votee.name)
        voteDict.append({"player": votee,
                         "votes": n})
    voteResults.append(voteDict)

def getVoteMajority(votes, tiebreaker = defaultTiebreaker):
    sortedVotes = sorted(votes, key=lambda x: x["votes"], reverse=True)
    if sortedVotes[0]["votes"] == sortedVotes[1]["votes"]:
        maxVotes = sortedVotes[0]["votes"]
        tied = [sortedVotes[0]["player"], sortedVotes[1]["player"]]
        for svote in sortedVotes[2:]:
            if svote["votes"] == maxVotes:
                tied.append(svote["player"])
            else:
                break
        return tiebreaker(tied)
    else:
        return sortedVotes[0]["player"]

def eliminate(player):
    eliminated.append(player)
    playerList.remove(player)
    print(player.name + " has been eliminated!")

def roundType1():
    getCompResults(playerList)
    vote([compResults[-1]["winner"]], [x for x in playerList if x != compResults[0]["loser"]])
    vote([compResults[-1]["loser"]], [x for x in playerList if x != compResults[0]["winner"] and x != getVoteMajority(voteResults[0])])
    getCompResults([compResults[-1]["loser"], getVoteMajority(voteResults[-1])])
    eliminate(compResults[-1]["loser"])

def roundType2():
    getCompResults(playerList)
    getCompResults([x for x in playerList if x != compResults[-1]["winner"]])
    eliminate(compResults[-1]["loser"])


junseok = Player("Junseok")
minseo = Player("Minseo")
minsoo = Player("Minsoo")
changyeop = Player("Changyeop")
jungmoon = Player("Jungmoon")
gura = Player("Gura")
poong = Player("Poong")
yuram = Player("Yuram")
eunji = Player("Eunji")
sunggyu = Player("Sunggyu")
sangmin = Player("Sangmin")
kyungran = Player("Kyungran")
jinho = Player("Jinho")
playerList = [junseok, minseo, minsoo, changyeop, jungmoon, gura, poong, yuram, eunji, sunggyu, sangmin, kyungran, jinho]

eliminated = []

roundList = [roundType1] * 10 + [roundType2]

for round in roundList:
    compResults = []
    voteResults = []
    round()

getCompResults(playerList)
print(compResults[-1]["winner"].name + " won!")
