import random

class Player:

    def __init__(self, name):
        self.name = name
        self.counters = {"votes": 1,
                         "position": 0}
        self.mins = {"votes": 0}

    def updateCounter(self, change, counter):
        if counter in self.mins.keys():
            if self.counters[counter] + change < self.mins[counter]:
                change = self.mins[counter] - self.counters[counter]
                print("The proposed change would bring " + counter + " below the minimum. Cutting off the change at " + str(change) + " instead.")
        self.counters[counter] += change
        return change

def getCompWinner(compPlayers):
    print("Competition between:")
    for player in compPlayers:
        print(player.name)
    haveWinner = False
    playerNames = [x.name for x in compPlayers]
    compDict = {}
    while not haveWinner:
        winName = input("Who won this competition?\n")
        if winName in playerNames:
            haveWinner = True
        else:
            print(winName + " is not playing this competition!")
    for player in compPlayers:
        if player.name == winName:
            winner = player
            compDict.update({"winner": winner})
    compResults.append(compDict)

def allocate(players, allocated):
    print("Allocation of " + allocated + ".")
    allocDict = []
    for player in players:
        numAlloc = int(input("How many " + allocated + " does " + player.name + " allocate?\n"))
        actAlloc = - player.updateCounter(- numAlloc, allocated)
        allocDict.append({"player": player,
                          "allocated": actAlloc})
    allocateResults.append(allocDict)

def directedVote(voters, votees, selfVote=False):
    print("Directed vote between:")
    for votee in votees:
        print(votee.name)
    voteeNames = [x.name for x in votees]
    forVotes = []
    againstVotes = []
    voteDict = []
    for voter in voters:
        haveVote = False
        while not haveVote:
            playerVote = input("Who does " + voter.name + " vote?\n")
            if not selfVote and playerVote == voter.name:
                print("You cannot vote for yourself!")
            elif playerVote in voteeNames:
                haveVote = True
            else:
                print(playerVote + " is not eligible to be voted for!")
        haveDecision = False
        while not haveDecision:
            decision = input("Is this vote for or against?\n")
            if decision in {"for", "For", "f", "F"}:
                haveDecision = True
                forVotes.append(playerVote)
            elif decision in {"against", "Against", "A", "a"}:
                haveDecision = True
                againstVotes.append(playerVote)
            else:
                print(decision + " is not a valid answer!")
    for votee in votees:
        n = forVotes.count(votee.name) - againstVotes.count(votee.name)
        voteDict.append({"player": votee,
                         "votes": n})
    voteResults.append(voteDict)

def getMin(players, metric, tiebreaker):
    sortedPlayers = sorted(players, key=lambda x: x.counters[metric], reverse=False)
    if sortedPlayers[0].counters[metric] == sortedPlayers[1].counters[metric]:
        minPos = sortedPlayers[0].counters[metric]
        tied = sortedPlayers[0:2]
        for splayer in sortedPlayers[2:]:
            if splayer.counters[metric] == minPos:
                tied.append(splayer)
            else:
                break
        return tiebreaker(tied)
    else:
        return sortedPlayers[0]

def lowVoteTiebreaker(tied):
    return getMin(tied, "votes", randomTiebreaker)

def randomTiebreaker(tied):
    return tied[random.randint(0, len(tied)-1)]

def eliminate(player):
    eliminated.append(player)
    playerList.remove(player)
    print(player.name + " has been eliminated!")

def roundType1():
    getCompWinner(playerList)
    compResults[0]["winner"].updateCounter(3, "votes")
    allocate(playerList, "votes")
    directedVoters = []
    for allocation in allocateResults[0]:
        directedVoters += [allocation["player"]] * allocation["allocated"]
    directedVote(directedVoters, playerList, True)
    for playerResult in voteResults[0]:
        playerResult["player"].updateCounter(playerResult["votes"], "position")
    eliminate(getMin(playerList, "position", lowVoteTiebreaker))


anna = Player("Anna")
bob = Player("Bob")
carrie = Player("Carrie")
dennis = Player("Dennis")
emma = Player("Emma")
frank = Player("Frank")
grace = Player("Grace")
harold = Player("Harold")
ivy = Player("Ivy")
jeff = Player("Jeff")
playerList = [anna, bob, carrie, dennis, emma, frank, grace, harold, ivy, jeff]

eliminated = []

roundList = [roundType1] * 9

for round in roundList:
    compResults = []
    allocateResults = []
    voteResults = []
    round()

winners = ""
if len(playerList) == 1:
    winners = playerList[0].name
else:
    for player in playerList[1:]:
        winners += player.name + ", "
    winners += "and " + playerList[0].name

print(winners + " won!")