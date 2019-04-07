import random

class Game:

    def __init__(self):
        self.playerList = []
        self.teamList = []
        self.eliminated = []
        self.voteResults = []
        self.compResults = []
        self.allocateResults = []

    def resetResults(self):
        self.voteResults = []
        self.compResults = []
        self.allocateResults = []
        for player in [x for x in self.playerList if "nominee" in x.affiliations]:
            player.removeAff("nominee")

    def getScoredTeamCompResults(self, compTeams):
        print("Team competition between:")
        for team in compTeams:
            print(team)
        compDict = {"scores": {}}
        for team in compTeams:
            pts = int(input("What did " + team + " score in this round?\n"))
            compDict["scores"].update({team: pts})
        compDict.update({"loser": min(compDict["scores"], key=compDict["scores"].get), "winner": max(compDict["scores"], key=compDict["scores"].get)})
        self.compResults.append(compDict)

    def getScoredCompResults(self, compPlayers):
        print("Competition between:")
        for player in compPlayers:
            print(player.name)
        compDict = {"scores": {}}
        for player in compPlayers:
            pts = int(input("What did " + player.name + " score in this round?\n"))
            compDict["scores"].update({player: pts})
        compDict.update({"loser": min(compDict["scores"], key=compDict["scores"].get), "winner": min(compDict["scores"], key=compDict["scores"].get)})
        for player in [x for x in self.playerList if x not in compPlayers]:
            compDict["scores"].update({player: 0})
        self.compResults.append(compDict)

    def getTeamCompResults(self, compTeams, winnerNeeded, loserNeeded):
        print("Competition between:")
        for team in compTeams:
            print(team)
        compDict = {}
        if winnerNeeded:
            haveWinner = False
            while not haveWinner:
                winName = input("Who won this round?\n")
                if winName in compTeams:
                    haveWinner = True
                else:
                    print(winName + " is not playing in this competition!")
            compDict.update({"winner": winName})
            if loserNeeded:
                if len(compTeams) > 2:
                    loserDetermined = False
                    while not loserDetermined:
                        loseName = input("Who lost this round?\n")
                        if loseName != winName and loseName in compTeams:
                            loserDetermined = True
                        elif loseName == winName:
                            print("Loser cannot be same as winner!\n")
                        else: 
                            print(loseName + " is not playing in this competition!")
                else:
                    for team in compTeams:
                        if team != winName:
                            loseName = team
                compDict.update({"loser": loseName})
        else:
            loserDetermined = False
            while not loserDetermined:
                loseName = input("Who lost this round?\n")
                if loseName in compTeams:
                    loserDetermined = True
                else: 
                    print(loseName + " is not playing in this competition!")
            compDict.update({"loser": loseName})
        self.compResults.append(compDict)

    def getCompResults(self, compPlayers, winnerNeeded, loserNeeded):
        print("Competition between:")
        for player in compPlayers:
            print(player.name)
        playerNames = [x.name for x in compPlayers]
        compDict = {}
        if winnerNeeded:
            haveWinner = False
            while not haveWinner:
                winName = input("Who won this round?\n")
                if winName in playerNames:
                    haveWinner = True
                else:
                    print(winName + " is not playing in this competition!")
            for player in compPlayers:
                if player.name == winName:
                    winner = player
                    compDict.update({"winner": winner})
            if loserNeeded:
                if len(compPlayers) > 2:
                    loserDetermined = False
                    while not loserDetermined:
                        loseName = input("Who lost this round?\n")
                        if loseName != winName and loseName in playerNames:
                            loserDetermined = True
                        elif loseName == winName:
                            print("Loser cannot be same as winner!\n")
                        else: 
                            print(loseName + " is not playing in this competition!")
                    for player in compPlayers:
                        if player.name == loseName:
                            loser = player
                else:
                    for player in compPlayers:
                        if player != winner:
                            loser = player
                compDict.update({"loser": loser})
        else:
            loserDetermined = False
            while not loserDetermined:
                loseName = input("Who lost this round?\n")
                if loseName in playerNames:
                    loserDetermined = True
                else: 
                    print(loseName + " is not playing in this competition!")
            for player in compPlayers:
                if player.name == loseName:
                    loser = player
            compDict.update({"loser": loser})
        self.compResults.append(compDict)

    def vote(self, voters, votees, selfVote=False):
        print("Vote between:")
        for votee in votees:
            print(votee.name)
        voteeNames = [x.name for x in votees]
        votes = []
        voteDict = {}
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
        for player in self.playerList:
            n = votes.count(player.name)
            voteDict.update({player: n})
        self.voteResults.append(voteDict)

    def nominate(self, nominators, numNominated, pool, selfVote=False):
        print("Nomination between:")
        for player in pool:
            print(player.name)
        poolNames = [x.name for x in pool]
        nominated = [x.name for x in self.playerList if "nominee" in x.affiliations]
        for nominator in nominators:
            for i in range(numNominated):
                haveNomination = False
                while not haveNomination:
                    nominee = input("Who does " + nominator.name + " nominate?\n")
                    if not selfVote and nominee == nominator.name:
                        print("You can't nominate yourself!")
                    elif nominee in poolNames and nominee not in nominated:
                        nominated.append(nominee)
                        haveNomination = True
                    else:
                        print(nominee + " is not eligible for nomination!")
        for player in pool:
            if player.name in nominated:
                player.addAff("nominee")

    def allocate(self, players, allocated):
        print("Allocation of " + allocated + ".")
        allocDict = {}
        for player in players:
            numAlloc = int(input("How many " + allocated + " does " + player.name + " allocate?\n"))
            actAlloc = - player.updateCounter(- numAlloc, allocated)
            allocDict.update({player: actAlloc})
        self.allocateResults.append(allocDict)

    def directedVote(self, voters, votees, selfVote=False):
        print("Directed vote between:")
        for votee in votees:
            print(votee.name)
        voteeNames = [x.name for x in votees]
        forVotes = []
        againstVotes = []
        voteDict = {}
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
        for player in self.playerList:
            n = forVotes.count(player.name) - againstVotes.count(player.name)
            voteDict.update({player: n})
        self.voteResults.append(voteDict)

    def swap(self, teams, newteams, players, numPreserve):
        for team in newteams:
            if team not in self.teamList:
                self.teamList.append(team)
        teamSizeDict = {}
        for team in teams:
            teamSize = 0
            for player in [x for x in players if team in x.affiliations]:
                player.removeAff(team)
                teamSize = teamSize + 1
            teamSizeDict.update({team: teamSize})
        if numPreserve:
            randomlyDivideTeamsOfSize(teamSizeDict, players)
        else:
            randomlyDivideTeams(teams + newteams, players)

    def merge(self, teams, newteam, players):
        if newteam not in self.teamList:
            self.teamList.append(newteam)
        for team in teams:
            for player in [x for x in players if team in x.affiliations]:
                player.removeAff(team)
                player.addAff(newteam)

    def eliminate(self, players):
        for player in players:
            self.eliminated.append(player)
            self.playerList.remove(player)
            print(player.name + " has been eliminated!")

    def juryVote(self, numJurors):
        self.vote(self.eliminated[(len(self.eliminated)-numJurors):], self.playerList)

class Player:

    def __init__(self, name, affiliations=[], counters=[]):
        self.name = name
        self.affiliations = []
        for aff in affiliations:
            self.affiliations.append(aff)
        self.counters = {}
        self.mins = {}
        self.maxs = {}
        for counter in counters:
            startVal = 0
            if "starts" in counter.keys():
                startVal = counter["starts"]
            self.counters.update({counter["counter"]: startVal})
            if "min" in counter.keys():
                self.mins.update({counter["counter"]: counter["min"]})
            if "max" in counter.keys():
                self.maxs.update({counter["counter"]: counter["max"]})

    def addAff(self, aff):
        if aff not in self.affiliations:
            self.affiliations.append(aff)

    def removeAff(self, aff):
        if aff in self.affiliations:
            self.affiliations.remove(aff)

    def updateCounter(self, change, counter):
        if counter in self.mins.keys():
            if self.counters[counter] + change < self.mins[counter]:
                change = self.mins[counter] - self.counters[counter]
                print("The proposed change would bring " + counter + " below the minimum. Cutting off the change at " + str(change) + " instead.")
        if counter in self.maxs.keys():
            if self.counters[counter] + change > self.maxs[counter]:
                change = self.maxs[counter] - self.counters[counter]
                print("The proposed change would bring " + counter + " above the maximum. Cutting off the change at " + str(change) + " instead.")
        self.counters[counter] += change
        return change
    
    def setCounter(self, val, counter):
        if counter in self.mins.keys() or counter in self.maxs.keys():
            if counter in self.mins.keys():
                if val < self.mins[counter]:
                    self.counters[counter] = self.mins[counter]
                    print("Attempt to set " + counter + " to value below the minimum. Setting to the minimum, " + str(self.mins[counter]) + "instead.")
            if counter in self.maxs.keys():
                if val > self.maxs[counter]:
                    self.counters[counter] = self.maxs[counter]
                    print("Attempt to set " + counter + " to value above the maximum. Setting to the maximum, " + str(self.maxs[counter]) + "instead.")
        else:
            self.counters[counter] = val
    
    def checkWinCondition(self, counter, goal):
        if self.counters[counter] >= goal:
                return True
        return False
        

def defaultTiebreaker(tied):
    badRock = random.randint(0, len(tied)-1)
    return tied[badRock]

def randomlyDivideTeams(teams, players):
    openTeams = teams.copy()
    counts = [0] * len(openTeams)
    numTeams = len(openTeams)
    for player in players:
        if len(openTeams) > 1:
            team = random.randint(0, len(openTeams)-1)
        else:
            team = 0
        player.addAff(openTeams[team])
        counts[team] = counts[team] + 1
        if counts[team] >= len(players) / numTeams:
            del openTeams[team]
            del counts[team]

def randomlyDivideTeamsOfSize(teams, players):
    openTeams = teams.copy()
    numTeams = len(openTeams.keys())
    counts = [0] * numTeams
    for player in players:
        if len(openTeams) > 1:
            team = random.randint(0, len(openTeams.keys())-1)
        else:
            team = 0
        player.addAff(openTeams.keys()[team])
        counts[team] = counts[team] + 1
        if counts[team] == openTeams[openTeams.keys()[team]]:
            openTeams.pop(openTeams.keys()[team])
            del counts[team]

def uses(players):
    deciders = ""
    if len(players) == 1:
        deciders = players[0].name
        firstword = "Does "
    else:
        for player in players[1:]:
            deciders += player.name + ", "
        deciders += "and " + players[0].name
        firstword = "Do "
    haveDecision = False
    while not haveDecision:
        decision = input(firstword + deciders + " use their power? yes/no?\n")
        if decision in {"yes", "y", "Yes", "Y"}:
            return True
        elif decision in {"no", "n", "No", "N"}:
           return False
        else:
            print(decision + " is not a valid answer!")

# minOrMax true for majority, false for minority
def getVoteMinOrMax(votes, minOrMax, tiebreaker = defaultTiebreaker):
    votedPlayer = max(votes, key=votes.get) if minOrMax else min(votes, key=votes.get)
    voteAmt = votes[votedPlayer]
    tied = []
    for p, v in votes.items():
        if v == voteAmt:
            tied += [p]
    if len(tied) > 1:
        return tiebreaker(tied)
    else:
        return votedPlayer

# minOrMax true for Max, false for min
def getMinOrMax(players, metric, minOrMax, tiebreaker=defaultTiebreaker):
    sortedPlayers = sorted(players, key=lambda x: x.counters[metric], reverse=minOrMax)
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

def randomDraw(num, pool):
    remainingPool = pool.copy()
    chosen = []
    for i in range(num):
        draw = random.randint(0, len(remainingPool)-1)
        chosen.append(remainingPool[draw])
        del remainingPool[draw]
    return chosen