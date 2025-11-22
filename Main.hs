module Main where

import BoxBot

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