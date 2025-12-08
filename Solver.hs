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
    search (legalMoves game) Nothing
  where
    search :: [Move] -> Maybe Move -> Move
    search [] (Just tieMove) = tieMove
    search [] Nothing        = head (legalMoves game)
    search (m:ms) tieSoFar =
      case makeMove game m of
        Nothing -> search ms tieSoFar
        Just g ->
          case whoWillWin g of
            Won p | p == turn -> m
            Tie               -> search ms (Just m)
            _                 -> search ms tieSoFar


