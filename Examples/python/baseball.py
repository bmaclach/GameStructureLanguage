class Player:

    def __init__(self, name, aff):
        self.name = name
        self.counters = {"points": 0}
        self.affiliations = [aff]

    def updateCounter(self, change, counter):
        self.counters[counter] += change
        return change

def getScoredTeamCompResults(compTeams):
    print("Team competition between:")
    for team in compTeams:
        print(team)
    compDict = {"scores": []}
    for team in compTeams:
        pts = int(input("What did " + team + " score in this round?\n"))
        compDict["scores"].append({"team": team,
                                   "score": pts})
    compResults.append(compDict)

def roundType1():
    getScoredTeamCompResults(teamList)
    for teamScore in compResults[0]["scores"]:
        teamPlayers = [x for x in playerList if teamScore["team"] in x.affiliations]
        for player in teamPlayers:
            player.updateCounter(teamScore["score"], "points")

jose = Player("Jose", "Jays")
kevin = Player("Kevin", "Jays")
john = Player("John", "Yankees")
joe = Player("Joe", "Yankees")
teamList = ["Jays", "Yankees"]
playerList = [jose, kevin, john, joe]

roundList = [roundType1] * 9

for round in roundList:
    compResults = []
    round()
    
sortedPlayers = sorted(playerList, key=lambda x: x.counters["points"], reverse=True)
for team in teamList:
    if team in sortedPlayers[0].affiliations:
        print(team + " won!")
        break
        