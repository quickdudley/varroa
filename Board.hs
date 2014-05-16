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

{-
           (0,5)                                     (5,5)
         (-1,4)                                        (5,4)
       (-2,3)                                            (5,3)
     (-3,2)                                                (5,2)
   (-4,1)  (-3,1) (-2,1) (-1,1) (0,1) (1,1) (2,1) (3,1) (4,1) (5,1)
(-5,0) (-4,0) (-3,0) (-2,0) (-1,0) (0,0) (1,0) (2,0) (3,0) (4,0) (5,0)
   (-5,-1)                                                    (4,-1)
     (-5,-2)                                                (3,-2)
       (-5,-3)                                            (2,-3)
         (-5,-4)                                        (1,-4)
           (-5,-5)                                    (0,-5)
-}

boardRange :: [(Int8,Int8)]
boardRange = bll ++ blh where
  bll = do
    y <- [-5..0]
    x <- [-5..y+5]
    return (x,y)
  blh = do
    y <- [1..5]
    x <- [y-5..5]
    return (x,y)

onBoard (x,y) =
  isqb x
  && isqb y
  && x >= (y - 5)
  && x <= (y + 5)
  where
    isqb n = n >= (-5) && n <= 5

