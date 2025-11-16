safeMakeMove :: Game -> Move -> Either String Game
safeMakeMove game@(edges, turn, boxes, size) move
  | moveExists move edges =
      Left "Illegal move: that edge has already been drawn."
  | move `notElem` legalMoves game =
      Left "Illegal move: that edge is outside the board."
  | otherwise =
      Right (newEdges, nextTurn turn, boxes, size)
  where
    newEdges = move : edges

safeCheckChamp :: Game -> Either String Winner
safeCheckChamp game
  | not (gameOver game) = Left "Game is not over yet."
  | otherwise = Right (checkChamp game)