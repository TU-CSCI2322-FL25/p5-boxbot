-- Type Classes

type Point = (Int, Int)
data Direction = Right | Down
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

makeMove :: Game -> Move -> Maybe Game
makeMove (edges, turn, boxes, size) move =
  if moveExists move edges
     then Nothing -- bascially the move isn't allowed idk how else to show it
  else
     let newEdges = move : edges
     in Just (newEdges, turn, boxes, size)