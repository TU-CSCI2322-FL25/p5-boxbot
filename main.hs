import Data.List

-- Type Classes

type Point = (Int, Int)
data Direction = DirRight | DirDown deriving (Eq, Show)
data Player = X | O
type Edge = (Point, Direction)
type Move = Edge
type Box = (Point, Player)
type Turn = Player
type Winner = Maybe Player -- idk
type Game = ([Edge], Turn, [Box], Int) -- int is a variable square size of the board

legalMoves :: Game -> [Move]
legalMoves (madeEdges, _, _, size) = allEdges \\ madeEdges
        where
                allEdges = rightEdges ++ downEdges
                rightEdges = [((x,y), DirRight) | x <- [1.. size - 1], y <- [1.. size]]
                downEdges = [((x,y), DirDown) | x <- [1.. size], y <- [1.. size - 1]]


-- index is 1 thru Size
-- function: lookup
-- function: change the players turn
-- function: (probably list comp) (or a fold idk) Scoreboard system would be defined as a function of scores
-- function: have a list of all moves, and then immediately subtract the move that any player makes