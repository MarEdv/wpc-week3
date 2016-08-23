module Main where

import WpcWeek3.NormalMazeSolver
import System.Environment(getArgs)

main :: IO ()
main = do
       args <- getArgs
       let fileName = head args
       textMaze <- loadFile fileName
       let maze = convertToMaze textMaze
       let movesList = findPath maze
       mapM_ putStrLn movesList
       putStrLn $ show (length movesList) ++ " steps"

