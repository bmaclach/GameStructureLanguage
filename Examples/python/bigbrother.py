from gamelib import *

def hohTiebreaker(tied):
    game.vote([x for x in game.playerList if "HOH" in x.affiliations], [x for x in game.playerList if "nominated" in x.affiliations])
    evictee = getVoteMinOrMax(game.voteResults[-1], True)
    del game.voteResults[-1] #tiebreaker vote results should not be included in vote history
    return evictee

def roundType1():
    game.getCompResults([x for x in game.playerList if not "HOH" in x.affiliations], True, False)
    for player in [x for x in game.playerList if "HOH" in x.affiliations]:
        player.removeAff("HOH")
        player.addAff("houseguest")
    game.compResults[0]["winner"].addAff("HOH")
    game.nominate([x for x in game.playerList if "HOH" in x.affiliations], 2, game.playerList)
    compPlayers = [x for x in game.playerList if "HOH" in x.affiliations or "nominated" in x.affiliations]
    for i in range(3):
        compPlayers.append(randomDraw([x for x in game.playerList if x not in compPlayers]))
    game.getCompResults(compPlayers, True, False)
    if uses(game.compResults[1]["winner"]):
        game.vote([game.compResults[1]["winner"]], [x for x in game.playerList if "nominated" in x.affiliations], True)
        saved = getVoteMinOrMax(game.voteResults[-1], True)
        game.nominate([x for x in game.playerList if "HOH" in x.affiliations], 1, [x for x in game.playerList if x != game.compResults[1]["winner"]])
        saved.removeAff("nominated")
        del game.voteResults[-1] # vote in branch should not be included in all votes
    game.vote([x for x in game.playerList if "HOH" not in x.affiliations and "nominated" not in x.affiliations], [x for x in game.playerList if "nominated" in x.affiliations])
    game.eliminate(getVoteMinOrMax(game.voteResults[-1], True, hohTiebreaker))
    for player in [x for x in game.playerList if "nominated" in x.affiliations]:
        player.removeAff("nominated")

def roundType2():
    game.getCompResults([x for x in game.playerList if not "HOH" in x.affiliations], True, False)
    for player in [x for x in game.playerList if "HOH" in x.affiliations]:
        player.removeAff("HOH")
        player.addAff("houseguest")
    game.compResults[0]["winner"].addAff("HOH")
    game.nominate([x for x in game.playerList if "HOH" in x.affiliations], 2, game.playerList)
    game.getCompResults(game.playerList, True, False)
    if uses(game.compResults[1]["winner"]):
        game.vote([game.compResults[1]["winner"]], [x for x in game.playerList if "nominated" in x.affiliations], True)
        saved = getVoteMinOrMax(game.voteResults[0], True)
        game.nominate([x for x in game.playerList if "HOH" in x.affiliations], 1, [x for x in game.playerList if x != game.compResults[1]["winner"]])
        saved.removeAff("nominated")
        del game.voteResults[-1] # vote in branch should not be included in all votes
    game.vote([x for x in game.playerList if "HOH" not in x.affiliations and "nominated" not in x.affiliations], [x for x in game.playerList if "nominated" in x.affiliations])
    game.eliminate(getVoteMinOrMax(game.voteResults[-1], True, hohTiebreaker))
    for player in [x for x in game.playerList if "nominated" in x.affiliations]:
        player.removeAff("nominated")

def roundType3():
    game.getCompResults(game.playerList, True, False)
    game.vote([game.compResults[0]["winner"]], game.playerList)
    game.eliminate(getVoteMinOrMax(game.voteResults[0], True))

game = Game()

game.playerList.append(Player("Lori", ["houseguest"]))
game.playerList.append(Player("Tanya", ["houseguest"]))
game.playerList.append(Player("Eric", ["houseguest"]))
game.playerList.append(Player("Josh", ["houseguest"]))
game.playerList.append(Player("Chiara", ["houseguest"]))
game.playerList.append(Player("Gerry", ["houseguest"]))
game.playerList.append(Player("Roddy", ["houseguest"]))
game.playerList.append(Player("Marcellas", ["houseguest"]))
game.playerList.append(Player("Amy", ["houseguest"]))
game.playerList.append(Player("Jason", ["houseguest"]))
game.playerList.append(Player("Danielle", ["houseguest"]))
game.playerList.append(Player("Lisa", ["houseguest"]))

roundList = [roundType1] * 6 + [roundType2] * 3 + [roundType3]

for round in roundList:
    game.resetResults()
    round()

game.juryVote(7)
winner = getVoteMinOrMax(game.voteResults[-1], True)
print(winner.name + " won!")