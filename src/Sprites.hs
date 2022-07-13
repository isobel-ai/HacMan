module Sprites (Direction (..), renderGhosts, renderPacMan, renderScaredGhosts) where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Juicy (loadJuicyPNG)

import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (fromMaybe)

import Display

{- PacMan can face a cardinal or default direction.
   * Enum is derived so randomly generated numbers can be mapped to Directions.
-}
data Direction = North | East | South | West | Default
  deriving (Eq, Enum)

-- Loads a picture from the given filepath.
loadPic :: FilePath -> Picture -> Picture
loadPic fp alt = fromMaybe alt $ unsafePerformIO (loadJuicyPNG fp)

-- There's a definition for each Picture used in the game that involves loading an image.
pacManPic :: Picture
pacManPic = scale (squareSizeF / pacManW) (squareSizeF / pacManH) $ loadPic pacManFile pacManDrawn

inkyPic :: Picture
inkyPic = scale (squareSizeF / inkyW) (squareSizeF / inkyH) $ loadPic inkyFile inkyDrawn

blinkyPic :: Picture
blinkyPic = scale (squareSizeF / blinkyW) (squareSizeF / blinkyH) $ loadPic blinkyFile blinkyDrawn

pinkyPic :: Picture
pinkyPic = scale (squareSizeF / pinkyW) (squareSizeF / pinkyH) $ loadPic pinkyFile pinkyDrawn

clydePic :: Picture
clydePic = scale (squareSizeF / clydeW) (squareSizeF / clydeH) $ loadPic clydeFile clydeDrawn

scaredPic :: Picture
scaredPic = scale (squareSizeF / scaredW) (squareSizeF / scaredH) $ loadPic scaredFile scaredDrawn

-- Renders PacMan to the given CoOrd, and rotates it based on the given Direction.
renderPacMan :: CoOrd -> Direction -> Picture
renderPacMan coO dir = translatePic coO (rotate angle pacManPic)
  where angle :: Float
        angle = case dir of
                  North -> 270  
                  West  -> 180 
                  South -> 90
                  _     -> 0

-- Functions for rendering the ghosts to the given CoOrds. One renders the regular images of the ghosts and the 
   other renders the scaredPic image for each ghost.
renderGhosts :: [CoOrd] -> [Picture]
renderGhosts coOrds = zipWith translatePic coOrds [inkyPic, blinkyPic, pinkyPic, clydePic] 

renderScaredGhosts :: [CoOrd] -> [Picture]
renderScaredGhosts coOrds = zipWith translatePic coOrds (replicate 4 scaredPic) 
