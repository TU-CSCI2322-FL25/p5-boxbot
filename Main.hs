module Main where 

import BoxBot
import Solver

import System.Environment
import System.Console.GetOpt

import Data.List
import Text.Read
import Data.List.Split
import Data.Maybe

data Flag = HelpFlag | MoveFlag String | VerboseFlag deriving (Show, Eq)


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

--main :: IO ()
--main = do
    --putStrLn "Enter game file path:"
    --path <- getLine
    --game <- loadGame path
    --putBestMove game




-- anything below this comment line is story 24-26. i tried 21-23 but i cant

options :: [OptDescr Flag]
options =
    [ Option ['h'] ["help"]    (NoArg HelpFlag)         "Show help"
    , Option ['m'] ["move"]    (ReqArg MoveFlag "<mv>") "Apply a move"
    , Option ['v'] ["verbose"] (NoArg VerboseFlag)      "Verbose output"
    ]

printHelp :: IO ()
printHelp = do
    putStrLn "Usage: program [options] <gamefile>"
    putStrLn "-h, --help        Show help"
    putStrLn "-m, --move <mv>   Apply a move"
    putStrLn "-v, --verbose     Pretty-print board and show outcome"

applyMoveString :: Game -> String -> Game
applyMoveString g mv =
    let (a,b) = break (==',') mv
        x = read a
        y = read (tail b)
    in makeMove g (x,y)

main :: IO ()
main = do
    args <- getArgs
    let (flags, files, _) = getOpt Permute options args

    if HelpFlag `elem` flags
       then printHelp
       else do
            if null files
               then error "No game file provided."
               else return ()

            game <- loadGame (head files)

            case find isMove flags of
                Just (MoveFlag mv) -> do
                    let g2 = applyMoveString game mv
                    if VerboseFlag `elem` flags
                       then do
                            putStrLn (drawGame g2)
                            putStrLn "Move applied."
                       else putStrLn (showGame g2)

                Nothing ->
                    if VerboseFlag `elem` flags
                       then do
                            let mv = bestMove game
                            putStrLn ("Move: " ++ show mv)
                            putStrLn ("Outcome: " ++ show (whoWillWin game))
                            putStrLn (drawGame (makeMove game mv))
                       else print (bestMove game)

isMove :: Flag -> Bool
isMove (MoveFlag _) = True
isMove _ = False
