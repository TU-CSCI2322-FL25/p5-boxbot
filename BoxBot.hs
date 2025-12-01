module BoxBot where

import Data.List
import Data.Maybe
import Text.Read
import Data.List.Split

-- Type Classes

type Point = (Int, Int)
data Direction = DirRight | DirDown deriving (Show, Eq)
data Player = X | O deriving (Show, Eq)
type Edge = (Point, Direction)
type Move = Edge
type Box = (Point, Player)
type Turn = Player
data Winner = Tie | Ongoing | Won Player deriving (Show, Eq)
type Game = ([Edge], Turn, [Box], Int) -- int is a variable square size of the board

gameOver :: Game -> Bool
gameOver game@(edges, turn, boxes, size) =
    length edges == 2 * (size - 1) * size

checkChamp :: Game -> Winner
checkChamp game@(edges, turn, boxes, size) = 
    if gameOver game
        then 
            let xCount = length [b | b <- boxes, snd b == X]
                oCount = length [b | b <- boxes, snd b == O]
            in if xCount > oCount 
               then Won X
               else if oCount > xCount
                    then Won O
                    else Tie
    else Ongoing

legalMoves :: Game -> [Move]
legalMoves (madeEdges, _, _, size) = allEdges \\ madeEdges
    where
        allEdges = rightEdges ++ downEdges
        rightEdges = [((x,y), DirRight) | x <- [1..size-1], y <- [1..size]]
        downEdges = [((x,y), DirDown) | x <- [1..size], y <- [1..size-1]]

moveExists :: Edge -> [Edge] -> Bool
moveExists m ex = m `elem` ex

opponent :: Player -> Player
opponent X = O
opponent O = X

withinBounds :: Move -> Int -> Bool
withinBounds ((x, y), dir) size =
  case dir of
    DirRight -> x < size && y <= size
    DirDown  -> y < size && x <= size

completedBoxes :: Game -> Move -> [Point]
completedBoxes (edges, _, _, size) ((x, y), dir) =
  case dir of
    DirRight -> filter finished [(x, y), (x, y-1)]
    DirDown  -> filter finished [(x, y), (x-1, y)]
  where
    finished (bx, by) =
      bx >= 1 && by >= 1 && bx < size && by < size &&
      ((bx, by), DirRight) `elem` edges &&
      ((bx, by), DirDown)  `elem` edges &&
      ((bx+1, by), DirDown) `elem` edges &&
      ((bx, by+1), DirRight) `elem` edges

makeMove :: Game -> Move -> Maybe Game
makeMove (edges, turn, boxes, size) move
   | moveExists move edges = Nothing
   | not (withinBounds move size) = Nothing
   | otherwise =
      Just (move : edges, if null finished then opponent turn else turn,
            boxes ++ [(p, turn) | p <- finished], size)
   where
      finished = completedBoxes (edges, turn, boxes, size) move