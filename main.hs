-- Type Classes

type Point = (Int, Int)
data Direction = DirRight | DirDown deriving (Eq, Show)
data Player = X | O
type Edge = (Point, Direction)
type Move = Edge
type Box = (Point, Player)
type Turn = Player
data Winner = Maybe Player -- idk
type Game = ([Edge], Turn, [Box], Int) -- int is a variable square size of the board


-- index is 1 thru Size
-- function: lookup
-- function: change the players turn
-- function: (probably list comp) (or a fold idk) Scoreboard system would be defined as a function of scores
-- function: have a list of all moves, and then immediately subtract the move that any player makes

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