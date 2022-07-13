module Maze where 

import Data.List ((\\))
import Data.Map (insert, fromList, Map, mapWithKey, (!?))

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture

import System.Random (randomRs)

import Display

-- The Maze is stored as a Map with CoOrd keys and Square values. 
newtype Maze = Maze {getMaze :: (Map CoOrd Square)}

{- Each Square in the Maze is in one of four states:
   - Boundary: the Square can not be entered by the ghosts or pac-man.
   - TouchedSquare: the Square can be entered by the ghosts or pac-man and pac-man has entered the square.
   - UntouchedSquare: the Square can be entered by the ghosts or pac-man and pac-man hasn't entered the square yet.
   - PowerSquare: an UntouchedSquare that contains a power pellet.
-}
data Square = Boundary | TouchedSquare | UntouchedSquare | PowerSquare
  deriving Eq

-- Determines where the pellets in the maze will be placed for the duration of the game.  
pellets :: [CoOrd]
pellets = zip (take 3 xs) (take 3 ys)
  where xs :: [Int]
        xs = randomRs (1,mazeW-2) (rGen 1)

        ys :: [Int]
        ys = randomRs (1,mazeH-2) (rGen 2) 

{- The Maze at the start of the game. A Maze of UnTouchedSqaures is generated then altered to add the Boundaries, 
   PowerSquares and a TouchedSquare starting Square.  
   * The TouchedSquare and Boundary conditions are before the PowerSquare condition as if a pellet is randomly 
     assigned to either of these squares, it doesn't actually get placed there. 
-}
startMaze :: Maze
startMaze = Maze $ mapWithKey alterSquare (getMaze untouchedSquareMaze)
  where alterSquare :: CoOrd -> Square -> Square
        alterSquare coOrd square 
          | coOrd == pacStart       = TouchedSquare
          | coOrd `elem` boundaries = Boundary
          | coOrd `elem` pellets    = PowerSquare
          | otherwise               = square
          where boundaries :: [CoOrd]
                boundaries = wallBoundaries ++ innerBoundaries
                  where wallBoundaries :: [CoOrd]
                        wallBoundaries = [(x,y) | x <- [0,mazeW-1], y <- [0..mazeH-1]] ++ 
                                         [(x,y) | x <- [0..mazeW-1], y <- [0,mazeH-1]]

                        innerBoundaries :: [CoOrd]
                        innerBoundaries = [(x,y) | x <- [2,17],                y <- [2..8] \\ [5]] ++ 
                                          [(x,y) | x <- [5,14],                y <- [1,2,8,9]] ++
                                          [(x,y) | x <- [7,12],                y <- [4..6]] ++ 
                                          [(x,y) | x <- [3,16] ++ [7..12],     y <- [2,8]] ++
                                          [(x,y) | x <- [4,5,7,8,11,12,14,15], y <- [4,6]] 

        untouchedSquareMaze :: Maze
        untouchedSquareMaze = Maze $ fromList [((x,y), UntouchedSquare) | x <- [0..mazeW-1], y <- [0..mazeH-1]]
                   

-- Converts the Maze into a Picture by converting the Squares into a list of Pictures.
renderMaze :: Maze -> Picture
renderMaze maze = pictures [renderSquare (x,y) | x <- [0..mazeW-1], y <- [0..mazeH-1]]
  where renderSquare :: CoOrd -> Picture
        renderSquare coOrd = translatePic coOrd squarePic
          where squarePic :: Picture
                squarePic = case getMaze maze !? coOrd of
                              Just TouchedSquare   -> Color pathCol (rectangleSolid squareSizeF squareSizeF)
                              Just UntouchedSquare -> Color pelletCol (circleSolid $ halfSquare / 2)
                              Just PowerSquare     -> Color pelletCol (circleSolid halfSquare)
                              _                    -> Color boundaryCol (rectangleSolid squareSizeF squareSizeF)

-- Changes a Square's type to TouchedSquare.
touchSquare :: CoOrd -> Maze -> Maze
touchSquare coOrd maze = Maze $ insert coOrd TouchedSquare (getMaze maze) 
