module Board (
 ) where

import qualified Data.Map as M
import Data.Int

data Player = Red | Green | Blue
  deriving (Enum,Bounded,Show)

data Direction = NE | EE | SE | SW | WW | NW
  deriving (Enum,Show)

type Board = M.Map (Int8,Int8) (Player,Direction)

{-
    (0,1)   (1,1)
(-1,0)  (0,0)  (1,0)
   (-1,-1)  (0,-1)
-}
step :: (Int8,Int8) -> Direction -> (Int8,Int8)
step (x,y) d = case d of
  NE -> (x+1,y+1)
  EE -> (x+1,y)
  SE -> (x,y-1)
  SW -> (x-1,y-1)
  WW -> (x-1,y)
  NW -> (x,y+1)

