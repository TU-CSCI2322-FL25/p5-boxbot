import Data.List
import Data.Maybe 
-- Type Classes

type Point = (Int, Int)
data Direction = DirRight | DirDown deriving (Show, Eq)
data Player = X | O deriving (Show, Eq)
type Edge = (Point, Direction)
type Move = Edge
type Box = (Point, Player)
type Turn = Player deriving (Show, Eq)
data Winner = Tie | Ongoing | Won Player deriving (Show, Eq) -- idk
type Game = ([Edge], Turn, [Box], Int) -- int is a variable square size of the board

drawGame :: Game -> String
drawGame (edges, _, boxes, size) = 
     unlines $ concat [[drawTop y, drawMiddle y] | y <- [1..size-1]] ++ [drawTop size]
   where
     drawTop y = concat [drawTopS x y | x <- [1..size-1]] ++ "*"
     drawTopS x y
        | ((x, y), DirRight) `elem` edges = "*---"
        | otherwise = "*   "
        
     drawMiddle y = concat [drawMiddleS x y | x <- [1..size-1]] ++ "|"
     drawMiddleS x y = 
        let leftWall = if ((x, y), DirDown) `elem` edges then "|" else " "
            boxChar = case lookup (x, y) boxes of
               Just X -> " X "
               Just O -> " O "
               Nothing -> "   "
        in leftWall ++ boxChar

--evaulating if, based on how much room is left on the board, any possible moves remain
gameOver :: Game -> Bool
gameOver game@(edges,turn,boxes,size) =
    length edges == 2*(size-1)*(size) 

--list comprehension to compare number of boxes between player X or player O. output the winner data type (logic to compare boxes)
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
                rightEdges = [((x,y), DirRight) | x <- [1.. size - 1], y <- [1.. size]]
                downEdges = [((x,y), DirDown) | x <- [1.. size], y <- [1.. size - 1]]

moveExists :: Edge -> [Edge] -> Bool
moveExists m ex = m `elem` ex

opponent :: Player -> Player
opponent X = O
opponent O = X

withinBounds :: Move -> Int -> Bool
withinBounds ((x, y), dir) size =
  case dir of
    Right -> x < size && y <= size
    Down  -> y < size && x <= size

completedBoxes :: Game -> Move -> [Point]
completedBoxes (edges, _, _, size) ((x, y), dir) =
  case dir of
    Right -> filter finished [(x, y), (x, y-1)]
    Down  -> filter finished [(x, y), (x-1, y)]
  where
    finished (bx, by) =
      bx >= 1 && by >= 1 && bx < size && by < size &&
      ((bx, by), Right) `elem` edges &&
      ((bx, by), Down)  `elem` edges &&
      ((bx+1, by), Down) `elem` edges &&
      ((bx, by+1), Right) `elem` edges


makeMove :: Game -> Move -> Maybe Game
makeMove (edges, turn, boxes, size) move
   | moveExists move edges = Nothing
   | not (withinBounds move size) = Nothing
   | otherwise =
      Just (move : edges, if null finished then opponent turn else turn,
            boxes ++ [(p, turn) | p <- finished], size)
   where
      finished = completedBoxes (edges, turn, boxes, size) move

whoWillWin :: Game -> Winner
whoWillWin game@(_, turn, _, _) = case checkChamp game of
  Tie -> Tie
  Winner p -> Winner p 
  Ongoing -> 
    let 
      moves = legalMoves game 
      games = catMaybe [makeMove game m | m <- moves] 
      winners = map whoWillWin games --Fairies 
    in
      if Won turn `elem` winners then Won turn 
      else if Tie `elem` winners then Tie 
      else Won opponent turn


bestMove :: Game -> Move
bestMove = undefined      