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

--Story 2
--Check who has won the game state, if anyone, with a function of type  Game -> Winner.

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
               then Just X
               else if oCount > xCount
                    then Just O
                    else Nothing  -- tie
    else Nothing 