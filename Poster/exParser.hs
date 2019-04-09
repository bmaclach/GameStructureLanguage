competition = do {reserved "scored"
                 ; cmp <- competitor
                 ; reserved "competition"
                 ; reserved "between"
                 ; il <- identifierList
                 ; return $ Scored cmp il}