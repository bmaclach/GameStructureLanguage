class Player:

    def __init__(self, name, aff):
        self.name = name
        self.points = 0
        self.affiliations = [aff]

def getCompResults(compTeams):
    print("Team competition between:")
    for team in compTeams:
        print(team)
    scores = []
    for team in compTeams:
        pts = int(input("What did " + team + " score in this round?\n"))
        scores.append(pts)
    return scores

jose = Player("Jose", "Jays")
kevin = Player("Kevin", "Jays")
john = Player("John", "Yankees")
joe = Player("Joe", "Yankees")
teamList = ["Jays", "Yankees"]
playerList = [jose, kevin, john, joe]

for i in range(9):
    results = getCompResults(teamList)
    for team, score in zip(teamList, results):
        teamPlayers = [x for x in playerList if team in x.affiliations]
        for player in teamPlayers:
            player.points += score
    
sortedPlayers = sorted(playerList, key=lambda x: x.points, reverse=True)
for team in teamList:
    if team in sortedPlayers[0].affiliations:
        print(team + " won!")
        break
        