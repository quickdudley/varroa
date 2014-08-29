module DrawBoard () where

import Board

import Data.Int
import Data.Function (on)
import Data.List
import qualified Data.Map as M
import Data.Array
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
    sequence_ $ zipWith goPoint (moveTo : repeat lineTo) cpts
    strokePreserve
    case M.lookup p b of
      Nothing -> return ()
      Just (player,direction) -> do
        paint
        let (pf1,pf2) = cnr2 ! direction
        sequence_ $ zipWith goPoint (moveTo : repeat lineTo) [c,pf1 c, pf2 c]
        case player of
          Red -> setSourceRGB 1 0 0
          Green -> setSourceRGB 0 1 0
          Blue -> setSourceRGB 0 0 1
        paint
        setSourceRGB 0 0 0
    return ()
   ) boardRange
  return ()
 where
  (sx,sy) = minimumBy (compare `on` fst) [
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

hex2grid x y = (x - 2*y + 10, 19 - 3*y)

