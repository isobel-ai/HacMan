module Display where

import Data.Time.Clock (utctDayTime, getCurrentTime)

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture

import System.IO.Unsafe (unsafePerformIO)
import System.Random (mkStdGen, StdGen)

-- Set the dimensions of the window.
windowSize :: (Int,Int)
windowSize = (600,450)
 
windowOffset :: (Int,Int)
windowOffset = (300,0) 

-- Set the dimensions of the Maze.
mazeW :: Int
mazeW = 20

mazeH :: Int
mazeH = 11

-- Set the dimensions of the game.
gameW :: Int
gameW = fst windowSize - 100

gameH :: Int
gameH = snd windowSize - 100

squareSizeI :: Int
squareSizeI = max gameW gameH `div` mazeW

squareSizeF :: Float
squareSizeF = fromIntegral squareSizeI

halfSquare :: Float
halfSquare = squareSizeF / 2

-- Set the game's colours.
backCol :: Color
backCol = black

boundaryCol :: Color
boundaryCol = blue

pathCol :: Color
pathCol = black

pelletCol :: Color
pelletCol = white

textCol :: Color
textCol = white

{- Set sprite properties.
   * Functions and definitions imported from Graphics.Gloss.Picture and Color in order to create and edit Pictures.
   * The ghostDrawn definition was made as each of the ghosts are drawn the same way, so the function reduces the
     amount of repeated code.
-}
pacManFile :: FilePath
pacManFile = "assets/PACMAN.png"

pacManW :: Float
pacManW = 1967

pacManH :: Float
pacManH = 2075

pacManDrawn :: Picture
pacManDrawn = scale (pacManW / squareSizeF) (pacManH / squareSizeF) $ Color yellow (circleSolid halfSquare)

pacStart :: CoOrd
pacStart = (1,1)

ghostDrawn :: Picture
ghostDrawn = Polygon [(negate halfSquare,halfSquare),(0,negate halfSquare),(halfSquare,halfSquare)]

inkyFile :: FilePath
inkyFile = "assets/INKY.png"

inkyW :: Float
inkyW = 300

inkyH :: Float
inkyH = 300

inkyDrawn :: Picture
inkyDrawn = scale (inkyW / squareSizeF) (inkyH / squareSizeF) $ Color cyan ghostDrawn
inkyStart :: CoOrd
inkyStart = (8,5)

blinkyFile :: FilePath
blinkyFile = "assets/BLINKY.png"

blinkyW :: Float
blinkyW = 300

blinkyH :: Float
blinkyH = 300

blinkyDrawn :: Picture
blinkyDrawn = scale (blinkyW / squareSizeF) (blinkyH / squareSizeF) $ Color red ghostDrawn

blinkyStart :: CoOrd
blinkyStart = (9,5)

pinkyFile :: FilePath
pinkyFile = "assets/PINKY.png"

pinkyW :: Float
pinkyW = 300

pinkyH :: Float
pinkyH = 300

pinkyDrawn :: Picture
pinkyDrawn = scale (pinkyW / squareSizeF) (pinkyH / squareSizeF) $ Color rose ghostDrawn

pinkyStart :: CoOrd
pinkyStart = (10,5)

clydeFile :: FilePath
clydeFile = "assets/CLYDE.png"

clydeW :: Float
clydeW = 225

clydeH :: Float
clydeH = 225

clydeDrawn :: Picture
clydeDrawn = scale (clydeW / squareSizeF) (clydeH / squareSizeF) $ Color orange ghostDrawn

clydeStart :: CoOrd
clydeStart = (11,5)

scaredFile :: FilePath
scaredFile = "assets/SCARED.png"

scaredW :: Float
scaredW = 180

scaredH :: Float
scaredH = 180

scaredDrawn :: Picture
scaredDrawn = scale (scaredW / squareSizeF) (scaredH / squareSizeF) $ Color (dark blue) ghostDrawn

{- Definitions/functions used in multiple files or files that depend on each other.
   - CoOrd defines the locations in the Maze as Int pairs.
   - rGen takes an input and uses it along with the current time of day to make a standard random generator.
   - translatePic is a higher order function that takes a CoOrd and turns it into a function that takes in a Picture
     and returns it translated by values determined by CoOrd.
   * where clauses are used to make the top-level functions more readable.
   * A mixture of pound operators and brackets are used to make the code clearer.
   * Functions from Data.Time.Clock, System.IO.Unsafe, System.Random and Graphics.Gloss.Picture are used to make 
     the functions more consise.
-}
type CoOrd = (Int, Int)

rGen :: Int -> StdGen
rGen x = mkStdGen (x * timeSeed)
  where timeSeed :: Int
        timeSeed = round $ utctDayTime $ unsafePerformIO getCurrentTime

translatePic :: CoOrd -> (Picture -> Picture)
translatePic (x,y) = translate xPos yPos
  where xPos :: Float
        xPos = fromIntegral $ x - (gameW `div` 2) + (x * squareSizeI)

        yPos :: Float
        yPos = fromIntegral $ y - (gameH `div` 2) + (y * squareSizeI)