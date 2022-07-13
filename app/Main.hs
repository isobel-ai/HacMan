{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Fixed (mod')
import Data.List ((\\), elemIndex)
import Data.Map ((!?), elems)
import Data.Maybe (fromMaybe)

import Graphics.Gloss (play, Display(InWindow))
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.Pure.Game (Event (..), Key (..), SpecialKey (..) )

import System.Random (randomRs)

import Display
import Maze
import Sprites

{- The game has five possible states.
   * State derives Eq as the equality instance for it is obvious.
-}
data State = StartGame | Play | Won | Lost | Finished
  deriving Eq

{- The ghosts have two possible states.
   * GhostState derives Eq as the equality instance for it is obvious.
-}
data GhostState = Normal | Scared
  deriving Eq

-- A record is used to store the values that represent the game's state.
data PacManGame = PacManGame { gameMaze :: Maze         -- The Maze the game is played in.
                             , pacManLoc :: CoOrd       -- The location of PacMan.
                             , pacManDir :: Direction   -- The direction of PacMan.
                             , inkyLoc :: CoOrd         -- The location of Inky.
                             , inkyRGen :: [Int]        -- The list of random numbers that determines Inky's path.
                             , blinkyLoc :: CoOrd       -- The location of Blinky.
                             , blinkyRGen :: [Int]      -- The list of random numbers that determines Blinky's path.
                             , pinkyLoc :: CoOrd        -- The location of Pinky.
                             , pinkyRGen :: [Int]       -- The list of random numbers that determines Pinky's path.
                             , clydeLoc :: CoOrd        -- The location of Clyde.
                             , clydeRGen :: [Int]       -- The list of random numbers that determines Clyde's path.
                             , state :: State           -- The State of the game.
                             , ghostState :: GhostState -- The state of the ghosts.
                             , pelletList :: [CoOrd]    -- The locations of the pellets in the game.
                             , score :: Int             -- The player's score for the level.
                             , scores :: [Int]          -- The list of the players score for each level played.
                             , level :: Int             -- The level of the game.
                             , moveTimer :: Float       -- The timer used to determine when the sprites move.
                             , movePeriod :: Float      -- The value that determines how often the sprites move.
                             }

{- Returns the CoOrds of the ghosts in the game's level.
   * RecordWildCards are used in this method to make accessing fields more concise.
   * take is used as it's a safe function - if the level isn't in the range 1-4 an empty list is returned.
   * This function is defined on the top level as it's used in multiple other functions. 
-}
getGhostLocs :: PacManGame -> [CoOrd] 
getGhostLocs game@PacManGame{..} = take level [inkyLoc,blinkyLoc,pinkyLoc,clydeLoc]

{- Determines the new location of a ghost (called when pacMan is in the same location as a ghost)
   * RecordWildCards are used in this method to make accessing fields more concise.
   * Guards are used over if or case statements as there are several choices and there's more than one type of
     condition.  
   * A where clause is used for definitions that aren't used anywhere but in this function. pacManIndex and 
     ghostIndex are generated the same way so a function is declared that can generate both of them, which makes
     their definitions clearer. fromMaybe is used in this method over fromJust as it's a safer function. Its 
     default value results in the ghost moving to Clyde's starting position.
   * This function is defined on the top level as it's used in multiple other functions. 
-}
newLoc :: PacManGame -> CoOrd -> CoOrd 
newLoc game@PacManGame{..} coOrd
  | pacManIndex /= ghostIndex = coOrd
  | pacManIndex == 0          = inkyStart
  | pacManIndex == 1          = blinkyStart
  | pacManIndex == 2          = pinkyStart
  | otherwise                 = clydeStart
  where coOrdIndex :: CoOrd -> Int
        coOrdIndex co = fromMaybe 3 (co `elemIndex` getGhostLocs game)
    
        pacManIndex :: Int 
        pacManIndex = coOrdIndex pacManLoc

        ghostIndex :: Int 
        ghostIndex = coOrdIndex coOrd

{- The game's starting state.
   * randomRs, imported from System.Random, is used to generate an infinite list of random numbers within the
     given range using the given standard generator. 
   * The chosen movePeriod makes sprites move about three times a second.
   * The moveTimer is set to equal the movePeriod so the sprites' first move occurs as soon as the user starts
     playing the game.
-}
startGame :: PacManGame
startGame = PacManGame { gameMaze = startMaze
                       , pacManLoc = pacStart
                       , pacManDir = Default       
                       , inkyLoc = inkyStart
                       , inkyRGen = randomRs (0,3) (rGen 1) 
                       , blinkyLoc = blinkyStart
                       , blinkyRGen = randomRs (0,3) (rGen 2)  
                       , pinkyLoc = pinkyStart
                       , pinkyRGen = randomRs (0,3) (rGen 3)
                       , clydeLoc = clydeStart
                       , clydeRGen = randomRs (0,3) (rGen 4)
                       , state = StartGame
                       , ghostState = Normal
                       , pelletList = pellets
                       , score = 10000
                       , scores = []
                       , level = 1
                       , movePeriod = 0.3                     
                       , moveTimer = movePeriod startGame
                       } 

{- Application entry point: allows the user to play the game.
   * The play function and InWindow data-type are imported from Graphics.Gloss.Interface.Pure.Game in order to run 
     the game. 
-}
main :: IO ()
main = play 
        (InWindow "Pac-Man" windowSize windowOffset)
        backCol
        60
        startGame
        renderGame 
        handleInput
        update

{- Renders the game by rendering the maze then rendering the sprites on top.
   * RecordWildCards are used in this method to make accessing fields more concise.
   * scale, color, text, Blank and pictures are imported from Graphics.Gloss.Data.Picture to create and edit 
     Pictures.
   * let-in clauses are used to define functions and defintions that are only used within the function, so 
     prevents the namespace from being clogged. The clauses are nested to make clear what functions/definitions
     work together.
   * The (.) operator is used to compose functions in editTextPic as it's a higher order function that outputs a
     function.
   * case statements are used where pattern matching can be used and there are more that two options; if-statements
     are used where there are two options, as it makes the code flow better.
   * A lambda function is used in the ghostPictures function to produce a function that outputs a list containing
     a Blank Picture regardless of input. This method is more consise than defining a full function.
-}
renderGame :: PacManGame -> Picture
renderGame game@PacManGame{..} = 
  let editTextPic :: Int -> Int -> (Picture -> Picture) 
      editTextPic tx ty = translatePic (tx,ty) . scale 0.25 0.25 . color textCol
        
      line1 :: String
      line1 = case state of
                StartGame -> "Use WASD to control PacMan."
                Play      -> "Level: " ++ show level ++ " Score: " ++ show score
                Won       -> "You Won! Your Score: " ++ show score
                Lost      -> "You Lost! Final Score: " ++ show (sum scores)
                Finished  -> "You Finished! Final Score: " ++ show (sum scores)

      line2 :: String
      line2 = let gameLevel :: String
                  gameLevel = if level == 1
                              then "Restart Game."
                              else "Play Level " ++ show level ++ "." 

              in if state == Won
                 then "Press Space to " ++ gameLevel
                 else "Press Space to Restart Game."

      ghostPictures :: [CoOrd] -> [Picture]
      ghostPictures = if state == Play || state == Lost
                      then if ghostState == Normal
                           then renderGhosts
                           else renderScaredGhosts
                      else (\_ -> [Blank])


    in pictures $ [renderMaze gameMaze, renderPacMan pacManLoc pacManDir, editTextPic 0 (mazeH + 3) (text line1)
                  ,editTextPic 0 (mazeH + 1) (text line2)] ++ ghostPictures (getGhostLocs game)
                                    
{- Responds to user input: WASD is used to control PacMan's direction and the spacebar to restart the game or move
   to the next level. Any other input is ignored.
   * Event, Key and SpecialKey are imported from Graphics.Gloss.Interface.Pure.Game in order to allow the user to
     interact with the game via the keyboard. 
   * Top-level pattern matching is used for this function as it makes the three distinct cases very clear.
   * RecordWildCards aren't used in the first version of the function as there are two PacManGame variables so
     using accessor methods without naming the PacManGame they are refering to would make the function unclear.
   * let-in and where clauses are used to define functions and definitions that are only used in the handleInput 
     function, which prevents clogging of the namespace. nested clauses are used to emphasise which functions and
     definitions correspond with each other.  
   * case statements are used where there are many options to choose from and the conditions can be pattern 
     matched, whereas if statements are used when there's two options.
-}
handleInput :: Event -> PacManGame -> PacManGame
handleInput (EventKey (Char c) _ _ _) game =
  let game' :: PacManGame
      game' = game {pacManDir = newDirection} 
        where newDirection :: Direction
              newDirection = case c of
                               'w' -> North
                               'd' -> East
                               's' -> South
                               'a' -> West
                               _   -> pacManDir game

  in case state game of
       StartGame -> game' {state = if pacManDir game' == Default
                                   then StartGame
                                   else Play
                          }
       _        -> game' 

handleInput (EventKey (SpecialKey KeySpace) _ _ _) game@PacManGame{..} = startGame {level = newLevel, scores = newScores}
  where newLevel :: Int
        newLevel = if state == StartGame || state == Won
                   then level
                   else 1
        
        newScores :: [Int] 
        newScores = if state == Finished || level == 1
                    then []
                    else scores

handleInput e game = game

{- Updates the PacManGame record where necessary.
   * The mod' and elems functions and the (\\) operator are imported from the Data.Fixed, Data.Map and Data.List
     respectively, in order to make the function more consise (by not having to redefine functions with the same
     functionality here). 
   * RecordWildCards are used in this method to make accessing fields more concise.
   * if statements are used throughout as they follow the logical flow of the selections they represent.
   * let-in clauses are used to define functions and definitions that are only ever called via the top-level
     function. Some definitions were made to remove repeated identical function calls (e.g. mazeSquares).
     Nested let-in clauses are used to emphasise where functions/definitions are used, however many of the lower
     level functions in the update function link together, so they're mostly left separate for clarity. 
-}
update :: Float -> PacManGame -> PacManGame
update timeElapsed game@PacManGame{..} = 
  if state == Play
  then let moveTime :: Float 
           moveTime = moveTimer + timeElapsed
          
           newGame :: PacManGame
           newGame = checkGameState {moveTimer = moveTime `mod'` movePeriod}

           checkGameState :: PacManGame
           checkGameState = if pacManLoc `elem` getGhostLocs game                                                                      
                            then if ghostState == Normal
                                 then game {state = Lost}
                                 else game {ghostState = Normal, score = score + 100
                                           ,inkyLoc = newLoc game inkyLoc, blinkyLoc = newLoc game blinkyLoc
                                           ,pinkyLoc = newLoc game pinkyLoc, clydeLoc = newLoc game clydeLoc}
                            else checkWinGame

           checkWinGame :: PacManGame
           checkWinGame = let mazeSquares :: [Square]
                              mazeSquares = elems (getMaze gameMaze)           
                          in if UntouchedSquare `elem` mazeSquares || PowerSquare `elem` mazeSquares
                             then checkGhostState 
                             else checkFinishGame {scores = score : scores}

           checkGhostState :: PacManGame
           checkGhostState = if pacManLoc `elem` pelletList
                             then game {ghostState = Scared, pelletList = pelletList \\ [pacManLoc]}
                             else game

           checkFinishGame :: PacManGame
           checkFinishGame = let nextLevel :: Int
                                 nextLevel = if level < 4 
                                             then level + 1
                                             else 1
                             in if level == 4
                                then game {state = Finished}
                                else game {state = Won, level = nextLevel} 

       in if moveTime > movePeriod 
          then move newGame
          else newGame

    else game

{- Moves PacMan based on the latest direction set and moves the ghosts randomly.
   * The (!?) operator is imported from Data.Map to find the Squares associated with input CoOrds in the Maze. It's
     used alongside the fromMaybe function in Data.Maybe. This is safer than the (!) operator and a more consise 
     method than defining a new function with the same purpose.
   * RecordWildCards are used in this method to make accessing fields more concise.
   * let-in clauses are used to define functions and definitions that are only used in the top-level move function,
     which prevents clogging of the namespace. Nested let-ins are used where functions/definitons chain together,
     but only if the formatting of the let-in block doesn't decrease the clarity of the overall function.
   * case statements are used where there are many options that can be pattern matched and if statements are used
     where there's two options.
   * The partial operator (!!) is used in randDir as randGen is an infinite list so any positive number input won't
     result in an error. Also, the randDir function is protected against negative input, further reducing the chance
     of the partial operator leading to the game crashing.
-}
move :: PacManGame -> PacManGame 
move game@PacManGame{..} = 
  let nextLocSquare :: CoOrd -> Square
      nextLocSquare coOrd = fromMaybe Boundary (getMaze gameMaze !? coOrd)
      
      nextLoc :: Direction -> CoOrd -> CoOrd
      nextLoc dir (cx,cy) = case dir of
                              North    -> (cx, cy+1)
                              East     -> (cx+1, cy)
                              South    -> (cx, cy-1)
                              West     -> (cx-1, cy)
                              Default  -> (cx, cy)

      nextPacManLoc :: CoOrd
      nextPacManLoc = nextLoc pacManDir pacManLoc 
                                
      game' :: Int -> PacManGame
      game' l = case l of 
                  1 -> game {score = score - 5, inkyLoc = nextRandLoc inkyLoc inkyRGen 0, inkyRGen = drop 1 inkyRGen}
                  2 -> (game' 1) {blinkyLoc = nextRandLoc blinkyLoc blinkyRGen 0, blinkyRGen = drop 1 blinkyRGen}
                  3 -> (game' 2) {pinkyLoc = nextRandLoc pinkyLoc pinkyRGen 0, pinkyRGen = drop 1 pinkyRGen}
                  _ -> (game' 3) {clydeLoc = nextRandLoc clydeLoc clydeRGen 0, clydeRGen = drop 1 clydeRGen}

      nextRandLoc :: CoOrd -> [Int] -> Int -> CoOrd
      nextRandLoc coOrd randGen n = let randDir :: Direction
                                        randDir = if n < 0
                                                  then toEnum (randGen !! 1)
                                                  else toEnum (randGen !! n)

                                        randLoc :: CoOrd
                                        randLoc = nextLoc randDir coOrd

                                    in if nextLocSquare randLoc == Boundary 
                                       then nextRandLoc coOrd randGen (n+1)
                                       else randLoc

      checkLoseGame :: PacManGame
      checkLoseGame = if nextPacManLoc `elem` getGhostLocs game 
                      then if ghostState == Normal
                           then (game' level) {state = Lost}
                           else (game' level) {pacManLoc = nextPacManLoc, score = score + 100
                                              ,inkyLoc = newLoc game inkyLoc, blinkyLoc = newLoc game blinkyLoc
                                              ,pinkyLoc = newLoc game pinkyLoc, clydeLoc = newLoc game clydeLoc}
                      else (game' level) {pacManLoc = nextPacManLoc}
      
  in if state == Play
     then if nextLocSquare nextPacManLoc == Boundary
          then game' level
          else checkLoseGame {gameMaze = touchSquare nextPacManLoc gameMaze}
     else game