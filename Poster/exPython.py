includeList1 = []; ident = game.playerList; 
idVal = ident; includeList1 += idVal
excludeList1 = []
idList1 = [x for x in includeList1 
    if x not in excludeList1]
game.getScoredCompResults(idList1)