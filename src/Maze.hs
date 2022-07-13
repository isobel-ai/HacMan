module Maze where 

import Data.List ((\\))
import Data.Map (insert, fromList, Map, mapWithKey, (!?))

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture

import System.Random (randomRs)

import Display

{- The Maze is stored as a Map with CoOrd keys and Square values. 
   * newtype is used over data here as it compliles more efficiently.
   * a record accessor is used to define the Maze constuctor to enable easier access to the Map inside the Maze. 
-}
newtype Maze = Maze {getMaze :: (Map CoOrd Square)}

{- Each Square in the Maze is in one of four states:
   - Boundary: the Square can not be entered by the ghosts or pac-man.
   - TouchedSquare: the Square can be entered by the ghosts or pac-man and pac-man has entered the square.
   - UntouchedSquare: the Square can be entered by the ghosts or pac-man and pac-man hasn't entered the square yet.
   - PowerSquare: an UntouchedSquare that contains a power pellet.
   * Square derives Eq as the equality instance for it is obvious.
-}
data Square = Boundary | TouchedSquare | UntouchedSquare | PowerSquare
  deriving Eq

{- Determines where the pellets in the maze will be placed for the duration of the game. 
   * The zip and take functions are used to construct a list of three CoOrds. This method is more elegant than 
     others like using list comprehensions as there are less symbols so the top-level function looks clearer.
   * A where clause is used as the definitions of xs and xy are quite long so the pellets function would look more
     cluttered if there was no seperation.
   * The randomRs function from System.Random is used to generate an infinite list of random numbers, but since
     Haskell is lazy, only two numbers from xs and xy are ever generated.
-} 
pellets :: [CoOrd]
pellets = zip (take 3 xs) (take 3 ys)
  where xs :: [Int]
        xs = randomRs (1,mazeW-2) (rGen 1)

        ys :: [Int]
        ys = randomRs (1,mazeH-2) (rGen 2) 

{- The Maze at the start of the game. A Maze of UnTouchedSqaures is generated then altered to add the Boundaries, 
   PowerSquares and a TouchedSquare starting Square. 
   * The mapWithKey and fromList functions are imported from Data.Map, as well as the (\\) operator from Data.List,
     so functions with the same functionality don't have to be written.
   * A mixture of pound operators and brackets are used in order to make associativity clear.
   * where clauses are used as the function and definitions needed to perform the toplevel function are only ever
     used here. This prevents clogging of the namespace. The where clauses are nested to make clear where each 
     function/definition is used. 
   * Guards are used over if and case statements for the alterSquare function as there are more than two options
     and different condition types are used within the block. The TouchedSquare and Boundary conditions are before 
     the PowerSquare condition as if a pellet is randomly assigned to either of these squares, it doesn't actually
     get placed there. 
   * List comprehensions are used to construct the boundaries list as a tuple is made for every combination of 
     values in the lists. This meant less repetition was required than if a different method (e.g. using zip)
     was used.
   * List comprehensions are also used in the untouchedSquareMaze definition as an alternative to nested zips, 
     which are more confusing than one clear comprehension.
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
                   

{- Converts the Maze into a Picture by converting the Squares into a list of Pictures.
   * The pictures alias from Graphics.Gloss.Data.Picture and the (!?) operator from Data.Map are used, rather than
     defining functions that do the same thing.
   * A list comprehension is used to make the list of Pictures rather than a zip as a function is applied to each
     zipped tuple. This means map would need to be used as well as zip, which would make the top-level function
     look more complicated.
   * where clauses are used as the function and definition needed to perform the toplevel function are only ever
     used here. This prevents clogging of the namespace. The where clauses are nested to make clear where each 
     function/definition is used. 
   * A case statement is used in the squarePic definition as the conditions of the block can be pattern matched.
     The '_' pattern is matched when the value is Just Boundary or Nothing.  
   * The (!?) operator was chosen over the unsafe (!!) operator in order to reduce the chance of the program ever
     crashing.
-}
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

{- Changes a Square's type to TouchedSquare.
   * The insert function from Data.Map is used as it's much more succint than other methods like unwrapping the map
     to a list, changing a value, and converting back to a map.
   * Both a pound operator and brackets are used in order to make associativity clear.
-}
touchSquare :: CoOrd -> Maze -> Maze
touchSquare coOrd maze = Maze $ insert coOrd TouchedSquare (getMaze maze) 