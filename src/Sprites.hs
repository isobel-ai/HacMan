module Sprites (Direction (..), renderGhosts, renderPacMan, renderScaredGhosts) where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Juicy (loadJuicyPNG)

import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (fromMaybe)

import Display

{- PacMan can face a cardinal or default direction.
   * Direction derives Eq as the equality instance for it is obvious.
   * Enum is derived so randomly generated numbers can be mapped to Directions.
-}
data Direction = North | East | South | West | Default
  deriving (Eq, Enum)

{- Loads a picture from the given filepath.
   * The fromMaybe, unsafePerformIO and loadJuicyPNG functions are imported from Data.Maybe, System.IO.Unsafe, and
     Graphics.Gloss.Juicy respectively, which made the function easier to put together.
   * fromMaybe was chosen over the less safe fromJust function so that the program is able to run using the alt
     Pictures if the filepath images don't work.
   * A pound operator as well as brackets are used to make associativity clear.
-}
loadPic :: FilePath -> Picture -> Picture
loadPic fp alt = fromMaybe alt $ unsafePerformIO (loadJuicyPNG fp)


{- There's a definition for each Picture used in the game that involves loading an image.
   * scale is imported from Graphics.Gloss.Data.Picture so the images used can be converted to Pictures of a 
     standard size.
   * Definitions are used instead of a single function that can construct any of the Pictures as this method means
     each image is only loaded from storage once, which increases performance speed of the game.
-}
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

{- Renders PacMan to the given CoOrd, and rotates it based on the given Direction.
   * rotate is imported from Graphics.Gloss.Data.Picture. It's applied to the Picture before the translatePic
     function as the rotation performed is about the origin so the Picture must also be at the origin for the
     rotation to leave it in the same place.
   * A where clause is used to define the angle as it's value is selected based on dir and having this selection
     block within the top-level function would make it look less elegant. 
   * A case-statement is used as the Direction can be pattern matched and there are many options. The '_' pattern
     is matched if dir is East or None.
-}
renderPacMan :: CoOrd -> Direction -> Picture
renderPacMan coO dir = translatePic coO (rotate angle pacManPic)
  where angle :: Float
        angle = case dir of
                  North -> 270  
                  West  -> 180 
                  South -> 90
                  _     -> 0

{- Functions for rendering the ghosts to the given CoOrds. One renders the regular images of the ghosts and the 
   other renders the scaredPic image for each ghost.
   * zipWith is used to perform the translatePic function on each corresponding CoOrd and Picture as it produces a 
     clear function definiton.
   * replicate is used in the renderScaredGhosts function rather than another method like typing 'scaredPic' four
     times or a list comprehension as it makes the function more concise.
-}
renderGhosts :: [CoOrd] -> [Picture]
renderGhosts coOrds = zipWith translatePic coOrds [inkyPic, blinkyPic, pinkyPic, clydePic] 

renderScaredGhosts :: [CoOrd] -> [Picture]
renderScaredGhosts coOrds = zipWith translatePic coOrds (replicate 4 scaredPic) 