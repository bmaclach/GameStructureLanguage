import random

class Player:

    def __init__(self, name):
        self.name = name
        self.affiliations = ["houseguest"]

    def addAff(self, aff):
        self.affiliations.append(aff)

    def removeAff(self, aff):
        self.affiliations.remove(aff)

def defaultTiebreaker(tied):
    badRock = random.randint(0, len(tied)-1)
    return tied[badRock]

def getCompWinner(compPlayers):
    print("Competition between:")
    for player in compPlayers:
        print(player.name)
    playerNames = [x.name for x in compPlayers]
    haveWinner = False
    compDict = {}
    while not haveWinner:
        winName = input("Who won this competition?\n")
        if winName in playerNames:
            haveWinner = True
        else:
            print(winName + " is not a player in this competition!")
    for player in compPlayers:
        if player.name == winName:
            winner = player
            compDict.update({"winner": winner})
    return compResults.append(compDict)

def nominate(nominators, numNominated, pool):
    print("Nomination between:")
    for player in pool:
        print(player.name)
    poolNames = [x.name for x in pool]
    nominated = [x for x in playerList if "nominated" in x.affiliations]
    for nominator in nominators:
        for i in range(numNominated):
            haveNomination = False
            while not haveNomination:
                nominee = input("Who does " + nominator.name + " nominate?\n")
                if nominee == nominator.name:
                    print("You can't nominate yourself!")
                elif nominee in poolNames and nominee not in nominated:
                    nominated.append(nominee)
                    haveNomination = True
                else:
                    print(nominee + " is not eligible for nomination!")
    for player in pool:
        if player.name in nominated:
            player.addAff("nominated")

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

def randomDraw(pool):
    draw = random.randint(0, len(pool)-1)
    return pool[draw]

def uses(player):
    haveDecision = False
    while not haveDecision:
        decision = input("Does " + player.name + " use their power? yes/no?\n")
        if decision in {"yes", "y", "Yes"}:
            return True
        elif decision in {"no", "n", "No"}:
           return False
        else:
            print(decision + " is not a valid answer!")

def hohTiebreaker(tied):
    vote([x for x in playerList if "HOH" in x.affiliations], [x for x in playerList if "nominated" in x.affiliations])
    evictee = getVoteMajority(voteResults[1])
    del voteResults[1] #tiebreaker vote results should not be included in vote history
    return evictee

def eliminate(player):
    eliminated.append(player)
    playerList.remove(player)
    print(player.name + " has been eliminated!")

def roundType1():
    getCompWinner([x for x in playerList if not "HOH" in x.affiliations])
    for player in [x for x in playerList if "HOH" in x.affiliations]:
        player.removeAff("HOH")
        player.addAff("houseguest")
    compResults[0]["winner"].addAff("HOH")
    nominate([x for x in playerList if "HOH" in x.affiliations], 2, playerList)
    compPlayers = [x for x in playerList if "HOH" in x.affiliations or "nominated" in x.affiliations]
    for i in range(3):
        compPlayers.append(randomDraw([x for x in playerList if x not in compPlayers]))
    getCompWinner(compPlayers)
    if uses(compResults[1]["winner"]):
        vote([compResults[1]["winner"]], [x for x in playerList if "nominated" in x.affiliations], True)
        saved = getVoteMajority(voteResults[0])
        nominate([x for x in playerList if "HOH" in x.affiliations], 1, [x for x in playerList if x != compResults[1]["winner"]])
        saved.removeAff("nominated")
        del voteResults[0] # vote in branch should not be included in all votes
    vote([x for x in playerList if "HOH" not in x.affiliations and "nominated" not in x.affiliations], [x for x in playerList if "nominated" in x.affiliations])
    eliminate(getVoteMajority(voteResults[0], hohTiebreaker))
    for player in [x for x in playerList if "nominated" in x.affiliations]:
        player.removeAff("nominated")

def roundType2():
    getCompWinner([x for x in playerList if not "HOH" in x.affiliations])
    for player in [x for x in playerList if "HOH" in x.affiliations]:
        player.removeAff("HOH")
        player.addAff("houseguest")
    compResults[0]["winner"].addAff("HOH")
    nominate([x for x in playerList if "HOH" in x.affiliations], 2, playerList)
    getCompWinner(playerList)
    if uses(compResults[1]["winner"]):
        vote([compResults[1]["winner"]], [x for x in playerList if "nominated" in x.affiliations], True)
        saved = getVoteMajority(voteResults[0])
        nominate([x for x in playerList if "HOH" in x.affiliations], 1, [x for x in playerList if x != compResults[1]["winner"]])
        saved.removeAff("nominated")
        del voteResults[0] # vote in branch should not be included in all votes
    vote([x for x in playerList if "HOH" not in x.affiliations and "nominated" not in x.affiliations], [x for x in playerList if "nominated" in x.affiliations])
    eliminate(getVoteMajority(voteResults[0], hohTiebreaker))
    for player in [x for x in playerList if "nominated" in x.affiliations]:
        player.removeAff("nominated")

def roundType3():
    getCompWinner(playerList)
    vote([compResults[0]["winner"]], playerList)
    eliminate(getVoteMajority(voteResults[0]))

lori = Player("Lori")
tanya = Player("Tanya")
eric = Player("Eric")
josh = Player("Josh")
chiara = Player("Chiara")
gerry = Player("Gerry")
roddy = Player("Roddy")
marcellas = Player("Marcellas")
amy = Player("Amy")
jason = Player("Jason")
danielle = Player("Danielle")
lisa = Player("Lisa")
playerList = [lori, tanya, eric, josh, chiara, gerry, roddy, marcellas, amy, jason, danielle, lisa]
eliminated = []

roundList = [roundType1] * 6 + [roundType2] * 3 + [roundType3]

for round in roundList:
    voteResults = []
    compResults = []
    round()

vote(eliminated[(len(eliminated)-7):], playerList)
winner = getVoteMajority(voteResults[1])
print(winner.name + " won!")