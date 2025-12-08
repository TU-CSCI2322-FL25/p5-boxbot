module Solver where

import BoxBot

import Data.List
import Text.Read
import Data.List.Split
import Data.Maybe

whoWillWin :: Game -> Winner
whoWillWin game@(_, turn, _, _) = case checkChamp game of
  Tie -> Tie
  Won p -> Won p
  Ongoing -> 
    let 
      moves = legalMoves game 
      games = catMaybes [makeMove game m | m <- moves] 
      winners = map whoWillWin games
    in
      if Won turn `elem` winners then Won turn 
      else if Tie `elem` winners then Tie 
      else Won (opponent turn)

bestMove :: Game -> Move
bestMove game@(edges, turn, boxes, size) =
  let
    moves = legalMoves game
    moveOutcomes =
      [(m, whoWillWin g) | m <- moves, Just g <- [makeMove game m]]
    winningMoves = [m | (m, Won p) <- moveOutcomes, p == turn]
    tyingMoves   = [m | (m, Tie)   <- moveOutcomes]
  in
    case winningMoves of
      (m:_) -> m
      []    -> case tyingMoves of
                (m:_) -> m
                []    -> fst (head moveOutcomes)

rateGame :: Game -> Int
rateGame game@(_, player, boxes, size) = 
  let 
    xCount = length [b | b <- boxes, snd b == X]
    oCount = length [b | b <- boxes, snd b == O]
    turnBonus = if player == X then 2 else -2
  in 
    if gameOver game
      then  
          if xCount > oCount then (size - 1) * size
          else if oCount > xCount then (size - 1) * size * (-1)
          else 0
    else xCount - oCount + turnBonus
  --If time: check for squares one away for completion, if so add additional turn bonus for each one

whoMightWin :: Game -> Int -> Int
whoMightWin game@(edges, turn, boxes, size) depth
    | depth == 0 = rateGame game 
    | gameOver game = rateGame game
    | otherwise = 
        let moves = legalMoves game
            games = [makeMove game m | m <- moves]
            validGames = [g | Just g <- games]
            scores = [whoMightWin g (depth - 1) | g <- validGames]
        in if Player == X 
            then maximum scores
            else minimum scores

goodMove :: Game Int -> Move
goodMove game@(edges, turn, boxes, size) depth = 
    let moves = legalMoves game
        moveScores = [(m, scoreMove m) | m <- moves]
        bestScore = if Player == X
                   then maximum [s | (_, s) <- moveScores]
                   else minimum [s | (_, s) <- moveScores]
        bestMoves = [m | (m, s) <- moveScores, s == bestScore]
    in head bestMoves
    where
        scoreMove m = case makeMove game m of
            Nothing -> if Player == X then -2 else 2
            Just g -> whoMightWin g (depth - 1)
