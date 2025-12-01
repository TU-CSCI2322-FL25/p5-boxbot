module Main where 

import BoxBot
import Solver

import Data.List
import Text.Read
import Data.List.Split
import Data.Maybe

drawGame :: Game -> String
drawGame (edges, _, boxes, size) = 
     unlines $ concat [[drawTop y, drawMiddle y] | y <- [1..size-1]] ++ [drawTop size]
   where
     drawTop y = concat [drawTopS x y | x <- [1..size-1]] ++ "*"
     drawTopS x y
        | ((x, y), DirRight) `elem` edges = "*---"
        | otherwise = "*   "
        
     drawMiddle y = concat [drawMiddleS x y | x <- [1..size-1]] ++ (if ((size, y), DirDown) `elem` edges then "|" else " ")
     drawMiddleS x y = 
        let leftWall = if ((x, y), DirDown) `elem` edges then "|" else " "
            boxChar = case lookup (x, y) boxes of
               Just X -> " X "
               Just O -> " O "
               Nothing -> "   "
        in leftWall ++ boxChar

readGame :: String -> Maybe Game
readGame s = 
     case lines s of 
          (sizeLine : turnLine : edgesLine : boxesLine : _) -> buildGame sizeLine turnLine edgesLine boxesLine
          (sizeLine : turnLine : edgesLine : _) -> buildGame sizeLine turnLine edgesLine "" 
          _ -> Nothing
     where
          readDir "R" = Just DirRight
          readDir "D" = Just DirDown
          readDir _   = Nothing
          readPlayer "X" = Just X
          readPlayer "O" = Just O
          readPlayer _   = Nothing
          buildGame sizeLine turnLine edgesLine boxesLine = 
               do size <- readMaybe sizeLine
                  turn <- case turnLine of
                             "X" -> Just X
                             "O" -> Just O
                             _   -> Nothing
                  edges <- sequence [ do let [a,b,c] = splitOn "," tok
                                         x <- readMaybe a
                                         y <- readMaybe b
                                         dir <- readDir c
                                         Just ((x, y), dir)
                                     | tok <- words edgesLine]
                  boxes <- sequence [ do let [a,b,c] = splitOn "," tok
                                         x <- readMaybe a
                                         y <- readMaybe b
                                         pl <- readPlayer c
                                         Just ((x, y), pl)
                                     | tok <- words boxesLine]         
                  Just (edges, turn, boxes, size)

showGame :: Game -> String
showGame (edges, turn, boxes, size) = 
     let showDir DirRight = "R"
         showDir DirDown = "D"
         showEdge ((x, y), d) = show x ++ "," ++ show y ++ "," ++ showDir d
         showBox ((x, y), p) = show x ++ "," ++ show y ++ "," ++ showPlayer p
         turnLine = showPlayer turn 
         edgesLine = unwords (map showEdge edges)
         boxesLine = unwords (map showBox boxes)
     in unlines [show size, turnLine, edgesLine, boxesLine]

showPlayer :: Player -> String
showPlayer X = "X"
showPlayer O = "O"

writeGame :: Game -> FilePath -> IO ()
writeGame game path = writeFile path (showGame game)

loadGame :: FilePath -> IO Game
loadGame path = do
    contents <- readFile path
    case readGame contents of
        Just game -> return game
        Nothing   -> error "Could not parse game file"

putBestMove :: Game -> IO ()
putBestMove game = do
    let move = bestMove game
        outcome = whoWillWin game
    putStrLn $ "Best move: " ++ show move
    putStrLn $ "Outcome: " ++ show outcome

main :: IO ()
main = do
    putStrLn "Enter game file path:"
    path <- getLine
    game <- loadGame path
    putBestMove game