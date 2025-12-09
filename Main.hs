module Main where 

import BoxBot
import Solver

import System.Environment
import System.Console.GetOpt
import System.Exit

import Data.List
import Text.Read
import Data.List.Split
import Data.Maybe

data Flag = HelpFlag 
          | MoveFlag String 
          | VerboseFlag 
          | WinnerFlag 
          | DepthFlag String 
          deriving (Show, Eq)


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
    [ Option ['h'] ["help"]    (NoArg HelpFlag)           "Show help"
    , Option ['m'] ["move"]    (ReqArg MoveFlag "<mv>")   "Apply a move"
    , Option ['v'] ["verbose"] (NoArg VerboseFlag)        "Verbose output"
    , Option ['w'] ["winner"]  (NoArg WinnerFlag)         "Use exhaustive search (best move)"
    , Option ['d'] ["depth"]   (ReqArg DepthFlag "<num>") "Set cutoff depth"
    ]

printHelp :: IO ()
printHelp = do
    putStrLn "Usage: program [options] <gamefile>"
    putStrLn "-h, --help          Show help"
    putStrLn "-m, --move <mv>     Apply a move"
    putStrLn "-v, --verbose       Pretty-print board and show outcome"
    putStrLn "-w, --winner        Use exhaustive search for best move"
    putStrLn "-d, --depth <num>   Set cutoff depth"

applyMoveString :: Game -> String -> Maybe Game
applyMoveString g mv = 
    let (a,b) = break (==',') mv
        x = read a
        y = read (tail b)
        (edges, turn, boxes, size) = g
        moveRight = ((x,y), DirRight)
        moveDown = ((x,y), DirDown)
    in if moveRight `elem` legalMoves g
        then makeMove g moveRight
        else if moveDown `elem` legalMoves g
            then makeMove g moveDown
            else Nothing

isMove :: Flag -> Bool
isMove (MoveFlag _) = True
isMove _ = False

isDepth :: Flag -> Bool
isDepth (DepthFlag _) = True
isDepth _ = False

getDepth :: [Flag] -> Int
getDepth flags = 
    case find isDepth flags of
        Just (DepthFlag d) -> case readMaybe d of
            Just n | n > 0 -> n
            _ -> 3  -- default if parse fails or invalid
        Nothing -> 3  -- default depth

describeOutcome :: Winner -> String
describeOutcome (Won X) = "X wins"
describeOutcome (Won O) = "O wins"
describeOutcome Tie = "Tie"
describeOutcome Ongoing = "Game ongoing"

main :: IO ()
main = do
    args <- getArgs
    let (flags, files, errs) = getOpt Permute options args

    -- check for errors
    if not (null errs)
       then do
           mapM_ putStrLn errs
           exitFailure
       else return ()

    -- handle help flag
    if HelpFlag `elem` flags
       then do
           printHelp
           exitSuccess
       else return ()

    -- check for game file
    if null files
       then do
           putStrLn "Error: No game file provided."
           putStrLn "Use -h or --help for usage information."
           exitFailure
       else return ()

    -- load game
    game <- loadGame (head files)

    let verbose = VerboseFlag `elem` flags
        useWinner = WinnerFlag `elem` flags
        depth = getDepth flags

    -- warn if both -w and -d are used
    if useWinner && isJust (find isDepth flags)
       then putStrLn "Warning: -d flag has no effect with -w flag"
       else return ()

    -- handle move flag
    case find isMove flags of
        Just (MoveFlag mv) -> do
            case applyMoveString game mv of
                Just newGame -> 
                    if verbose
                       then do
                            -- print with -v
                            putStrLn (drawGame newGame)
                       else do
                            -- print without -v
                            putStr (showGame newGame)
                Nothing -> do
                    putStrLn $ "Error: Invalid move '" ++ mv ++ "'"
                    exitFailure


        Nothing -> do
            let move = if useWinner
                      then bestMove game      -- exhaustive search
                      else goodMove game depth -- depth limited

            if verbose
               then do
                    -- verbose output with outcome
                    case makeMove game move of
                        Just newGame -> do
                            let outcome = whoWillWin game
                            putStrLn $ "Move: " ++ show move
                            putStrLn $ "Outcome: " ++ describeOutcome outcome
                            putStrLn (drawGame newGame)
                        Nothing -> do
                            putStrLn "Error: Computed move is invalid"
                            exitFailure
               else do
                    -- just output the move
                    print move