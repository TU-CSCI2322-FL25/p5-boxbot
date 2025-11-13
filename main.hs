-- Type Classes

type Point = (Int, Int)
data Direction = DirRight | DirDown deriving (Show, Eq)
data Player = X | O
type Edge = (Point, Direction)
type Move = Edge
type Box = (Point, Player)
type Turn = Player
data Winner = Maybe Player -- idk
type Game = ([Edge], Turn, [Box], Int) -- int is a variable square size of the board

-- print empty grid
--PrintEmptyGrid :: Game -> String
--PrintEmptyGrid = undefined
-- update grid
--UpdateGrid :: Game -> Move -> String
--UpdateGrid = undefined

Showline :: Game -> String
Showline (edges, _, boxes, size) = 
     unlines $ concat [[drawTop y, drawMiddle y] | y <- [1..size-1]] ++ [drawBottom]
   where
     drawTop y = concat [drawTopS x y | x <- [1..size]] ++ "*"
     drawTopS x y
        | ((x, y), DirRight) `elem` edges = "*---"
        | otherwise = "*   "
        
     drawMiddle y = concat [drawMiddleS x y | x <- [1..size]] ++ "|"
     drawMiddleS x y = 
        let leftWall = if ((x, y), DirDown) `elem` edges then "|" else " "
            boxChar = case lookup (x, y) boxes of
               Just X -> " X "
               Just O -> " O "
               Nothing -> "   "
        in leftWall ++ boxChar

     drawBottom = concat [drawBottomS x | x <- [1..size]] ++ "*"
     drawBottomS x 
        | ((x, size), DirRight) `elem` edges = "*---"
        | otherwise = "*   "


-- index is 1 thru Size
-- function: lookup
-- function: change the players turn
-- function: (probably list comp) (or a fold idk) Scoreboard system would be defined as a function of scores
-- function: have a list of all moves, and then immediately subtract the move that any player makes
