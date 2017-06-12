module DrawBoard (
  PlayState(..),
  renderBoard
 ) where

import Board

import Data.Int
import Data.Function (on)
import Data.List
import qualified Data.Map as M
import Data.Array
import Control.Monad
import Graphics.Rendering.Cairo

data PlayState = Ready Int8 | Selected Int8 (Int8,Int8) | Waiting
  deriving (Eq,Show)

renderBoard :: Double -> Double -> PlayState -> Board -> Render ()
renderBoard w h s b = do
  setSourceRGB 1 1 1
  paint
  setLineWidth 5
  setSourceRGB 0 0 0
  setLineCap LineCapRound
  mapM_ (\p -> do
    let
      c = uncurry hex2grid p
      cpts = map ($ c) corners
    zipWithM_ goPoint (moveTo : repeat lineTo)
      (cpts ++ [head cpts])
    case M.lookup p b of
      Nothing -> stroke
      Just (player,direction) -> do
        strokePreserve
        fill
        let (pf1,pf2) = cnr2 ! direction
        case player of
          Red -> setSourceRGB 1 0 0
          Green -> setSourceRGB 0 1 0
          Blue -> setSourceRGB 0 0 1
        zipWithM_ goPoint (moveTo : repeat lineTo)
          [c, pf1 c, pf2 c, c]
        fill
        setSourceRGB 0 0 0
    return ()
   ) boardRange
  return ()
 where
  (sx,sy) = minimum [
    let x = w / 22 in (x, x / (sqrt 0.75 * 2)),
    let y = h / 34 in (y * (sqrt 0.75 * 2), y)
   ]
  goPoint f (x,y) = f (sx * fromIntegral x) (sy * fromIntegral y)

cnr2 = array (NE,NW) $ zip [NE .. NW] $ zip corners $ tail (cycle corners)

corners = [
  \(x,y) -> (x,y-2),
  \(x,y) -> (x+1,y-1),
  \(x,y) -> (x+1,y+1),
  \(x,y) -> (x,y+2),
  \(x,y) -> (x-1,y+1),
  \(x,y) -> (x-1,y-1)
 ]

hex2grid x y = (2 * x - y + 11, 17 - 3 * y)

