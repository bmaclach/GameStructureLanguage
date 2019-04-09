compileComp (Scored Individual il) = do
    ildoc <- compileIdentifierList il 1
    return $ (vcat [fst ildoc, 
    text "game.getScoredCompResults" <> 
    parens (text "idList1")], snd ildoc)