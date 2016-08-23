module WpcWeek3.NormalMazeSolver (findPath,
  findStart,
  convertToMaze,
  loadFile,
  Square(..),
  defJunction
) where

import Control.Monad

data Square = Start {
                     top :: Square
                    ,right :: Square
                    ,bottom :: Square
                    ,left :: Square
                   }
                   |
                   End
                   |
                   Junction {
                     top :: Square
                    ,right :: Square
                    ,bottom :: Square
                    ,left :: Square
                   }
                   |
                   Wall
  deriving (Show, Eq, Ord)

type Maze = Square

{-
 This method makes it possible to create a Junction without specifying all its
 adjacent squares that are Walls.
-}
defJunction :: Square
defJunction = Junction Wall Wall Wall Wall

getLines :: String -> IO [String]
getLines = liftM lines . readFile

{-
 Loads a file into a text representation of a maze.
-}
loadFile :: String -> IO [String]
loadFile fileName = getLines fileName

{-
 Takes a text representation of the maze and converts it into the Maze datastructure.
-}
convertToMaze :: [String] -> Maze
convertToMaze rows = maze where
  starts = findStart rows
  maze = createSquare rows (head starts) []

{-
 Creates a Square from a coordinate in the text representation of the maze.
 It recursively creates Squares for the current Square's directly adjacent
 coordinates, using the method "createSquareSafe".
-}
createSquare :: [String] -> (Int, Int) -> [(Int, Int)] -> Square
createSquare rows (x, y) visited = result where
 css = createSquareSafe rows visited
 result = case (rows !! y) !! x of
  'X' -> End
  'O' -> Start
            (css (x, y-1))
            (css (x+1, y))
            (css (x, y+1))
            (css (x-1, y))
  ' ' -> Junction
            (css (x, y-1))
            (css (x+1, y))
            (css (x, y+1))
            (css (x-1, y))
  '#' -> Wall
  _ -> Wall

{-
 Creates a Square, if the coordinate is within the text representation of the maze.
 If it is outside, it will be considered a wall square.
-}
createSquareSafe :: [String] -> [(Int, Int)] -> (Int, Int) -> Square
createSquareSafe rows visited (x, y)
  | y < 0 = Wall
  | y >= length rows = Wall
  | x < 0 = Wall
  | x >= length (rows !! y) = Wall
  | (x,y) `elem` visited = Wall
  | otherwise = createSquare rows (x, y) (visited ++ [(x, y)])


{-
 Finds the coordinate of the start square in the string represesntation of the maze.
 It loops through the rows and finds all start points.
-}
findStart :: [String] -> [(Int, Int)]
findStart textMaze = startPoints where
  indexedRows = zip [0..(length textMaze) - 1] textMaze
  points = map findStartInRow indexedRows
  startPoints = concat points

{-
 Finds all start points in a string representation of a row of the maze.
-}
findStartInRow :: (Int, String) -> [(Int, Int)]
findStartInRow (y, row) = startPoints where
  indexedRow = zip [0..(length row)-1] row
  points = filter (\(_, square) -> square == 'O') indexedRow
  startPoints = map (\(x,_) -> (x, y)) points

{-
 A recursive, depth-first search that returns a list of steps to take
 from the starting point to the end point expressed in terms of east,
 west, south and north,
-}
findPath :: Maze -> [String]
findPath maze = case findPathRec maze of (_,x) -> x

{-
 Helper method that handles each Square type.
 - Wall means we hit a dead end.
 - Start means we try any of its adjacent squares
 - End means we found our goal
 - Junction means we try any of its adjacent squares
-}
findPathRec :: Square -> (Bool, [String])
findPathRec square = case square of
  Start t r b l -> processSteps steps where
    steps = zip ["north", "east", "south", "west"] [t, r, b, l]
  Junction t r b l -> processSteps steps where
    steps = zip ["north", "east", "south", "west"] [t, r, b, l]
  End -> (True, [])
  Wall -> (False, [])


{-
 Takes a list of tuples (direction, square). As soon as we find the end square,
 we collect the steps taken in a list and returns it along with a flag saying that
 we found the end square.
-}
processSteps :: [(String, Square)] -> (Bool, [String])
processSteps [] = (False, [])
processSteps ((direction, square):steps) = case findPathRec square of
      (True, ys) -> (True, direction : ys)
      (False, _) -> processSteps steps

