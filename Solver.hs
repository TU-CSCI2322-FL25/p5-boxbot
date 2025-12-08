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
