import random

class Player:

    def __init__(self, name):
        self.name = name
        self.votesAgainst = 0
        self.affiliations = []

    def addAff(self, aff):
        self.affiliations.append(aff)

    def removeAff(self, aff):
        self.affiliations.remove(aff)

def defaultTiebreaker(tied):
    badRock = random.randint(0, len(tied)-1)
    return tied[badRock]

def getTeamCompResults(compTeams):
    print("Team competition between:")
    for team in compTeams:
        print(team)
    haveLoser = False
    compDict = {}
    while not haveLoser:
        loser = input("Who lost this competition?\n")
        if loser in compTeams:
            haveLoser = True
            compDict.update({"loser": loser})
        else:
            print(loser + " are not playing this competition!")
    compResults.append(compDict)

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

def vote(voters, votees):
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
            if playerVote == voter.name:
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

def reVoteTiebreaker(tied):
    voters = [x for x in playerList if compResults[0]["loser"] in x.affiliations and x not in tied]
    vote(voters, tied)
    tiebroken = getVoteMajority(voteResults[1], pastVotesTiebreaker)
    del voteResults[1] #Tie vote results should not be saved in round vote history
    return tiebroken

def pastVotesTiebreaker(tied):
    tiebroken = getMaxVotesAgainst(tied, rocksTiebreaker)
    return tiebroken

def getMaxVotesAgainst(players, tiebreaker):
    sortedPlayers = sorted(players, key=lambda x: x.votesAgainst, reverse=True)
    if sortedPlayers[0].votesAgainst == sortedPlayers[1].votesAgainst:
        maxVotes = sortedPlayers[0].votesAgainst
        tied = sortedPlayers[0:2]
        for splayer in sortedPlayers[2:]:
            if splayer.votesAgainst == maxVotes:
                tied.append(splayer)
            else:
                break
        return tiebreaker(tied)
    else:
        return sortedPlayers[0]

def rocksTiebreaker(tied):
    atRisk = [x for x in playerList if compResults[0]["loser"] in x.affiliations and x not in tied]
    badRock = random.randint(0, len(atRisk)-1)
    return atRisk[badRock]

def eliminate(player):
    eliminated.append(player)
    playerList.remove(player)
    print(player.name + " has been eliminated!")

def swap(teams):
    for team in teams:
        for player in [x for x in playerList if team in x.affiliations]:
            player.removeAff(team)
    randomlyDivideTeams(teams, playerList)

def merge(teams):
    for team in teams:
        for player in [x for x in playerList if team in x.affiliations]:
            player.removeAff(team)

def reVoteTiebreaker2(tied):
    voters = [x for x in playerList if x not in tied]
    vote(voters, tied)
    tiebroken = getVoteMajority(voteResults[1], pastVotesTiebreaker2)
    del voteResults[1] #Tie vote results should not be saved in round vote history
    return tiebroken

def pastVotesTiebreaker2(tied):
    tiebroken = getMaxVotesAgainst(tied, rocksTiebreaker2)
    return tiebroken

def rocksTiebreaker2(tied):
    atRisk = [x for x in playerList if x != compResults[0]["winner"] and x not in tied]
    badRock = random.randint(0, len(atRisk)-1)
    return atRisk[badRock]

def roundType1():
    getTeamCompResults(teamList)
    loser = compResults[0]["loser"]
    vote([x for x in playerList if loser in x.affiliations], [x for x in playerList if loser in x.affiliations])
    eliminate(getVoteMajority(voteResults[0], reVoteTiebreaker))
    for playerResults in voteResults[0]:
        if playerResults["player"] in [x for x in playerList if loser in x.affiliations]:
            playerResults["player"].votesAgainst += playerResults["votes"]

def roundType2():
    swap(["Kucha", "Ogakor"])
    roundType1()

def roundType3():
    getCompWinner(playerList)
    vote(playerList, [x for x in playerList if x != compResults[0]["winner"]])
    eliminate(getVoteMajority(voteResults[0], reVoteTiebreaker2))
    for playerResults in voteResults[0]:
        playerResults["player"].votesAgainst += playerResults["votes"]

def roundType4():
    merge(["Kucha", "Ogakor"])
    roundType3()


debb = Player("Debb")
kel = Player("Kel")
maralyn = Player("Maralyn")
mitchell = Player("Mitchell")
kimmi = Player("Kimmi")
michael = Player("Michael")
jeff = Player("Jeff")
alicia = Player("Alicia")
jerri = Player("Jerri")
nick = Player("Nick")
amber = Player("Amber")
rodger = Player("Rodger")
elizabeth = Player("Elizabeth")
keith = Player("Keith")
colby = Player("Colby")
tina = Player("Tina")
teamList = ["Kucha", "Ogakor"]
playerList = [debb, kel, maralyn, mitchell, kimmi, michael, jeff, alicia, jerri, nick, amber, rodger, elizabeth, keith, colby, tina]
randomlyDivideTeams(teamList, playerList)

eliminated = []

roundList = [roundType1] * 4 + [roundType2] + [roundType1] + [roundType4] + [roundType3] * 7

for round in roundList:
    voteResults = []
    compResults = []
    round()

vote(eliminated[(len(eliminated)-7):], playerList)
winner = getVoteMajority(voteResults[1])
print(winner.name + " won!")
