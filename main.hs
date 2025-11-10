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
Showline = undefined
            -- even x = ""
            -- odd x = ""


-- index is 1 thru Size
-- function: lookup
-- function: change the players turn
-- function: (probably list comp) (or a fold idk) Scoreboard system would be defined as a function of scores
-- function: have a list of all moves, and then immediately subtract the move that any player makes
