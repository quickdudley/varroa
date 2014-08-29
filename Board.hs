module Board (
  Player(..),
  Direction(..),
  Step(..),
  Board,
  step,
  onBoard,
  boardRange,
  genMoves,
  playersFrom,
  rotateBoard,
  flipBoard,
  startBoard2,
  startBoard3,
  whichPlayers,
  whichPlayersFrom
 ) where

import Data.Ix
import Control.Monad (guard)
import Data.List (nub,intersect)
import qualified Data.Map as M
import Data.Int
import Data.Maybe (catMaybes)

data Player = Red | Green | Blue
  deriving (Eq,Enum,Bounded,Show,Ord)

data Direction = NE | EE | SE | SW | WW | NW
  deriving (Eq,Enum,Ord,Ix,Bounded,Show)

data Step = SL | SF | SR deriving (Enum,Show)

type Board = M.Map (Int8,Int8) (Player,Direction)

turn :: Int -> Direction -> Direction
turn n = toEnum . (`mod` 6) . (+ n) . fromEnum

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

playersFrom Blue = [Blue,Green,Red]
playersFrom Green = [Green,Red,Blue]
playersFrom Red = [Red,Blue,Green]

opposing :: Direction -> Direction
opposing = turn 3

{-
In the original Vala implementation, I swapped the x and y coordinates due to a
coding error, and put (0,0) in the Southwest corner. In this implementation
(0,0) is in the center, and the corners are at these coordinates:

(-5,-5), (0,-5), (5,0), (5,5), (0,5), and (-5,0)
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

-- Lay out the player's pieces at Blue's position, the functions for the real
-- starting positions will call this then rotate appropriately.
startp' :: Player -> Board
startp' player = M.fromList $ do
  y <- [-5,-4]
  x <- [-5 .. -3] ++ [y+3..y+5]
  return ((x,y), if x <= (-3)
    then (player,NE)
    else (player,NW))

{-
The matrix representing a 1/6 turn in this coordinate system is:
⎛ 1 1⎞
⎝-1 0⎠
Multiplying it by itself gives the matrices represented in 4-tuples below.
-}
rotateBoard :: Int -> Board -> Board
rotateBoard 0 b = b
rotateBoard n bd = (M.fromList . map rotate1 . M.toList) bd where
  rotate1 (t,(p,d)) = (rt t,(p,rd d))
  rt (x,y) = (a*x+c*y, b*x+d*y)
  (a,b,c,d) = case n `mod` 6 of
    0 -> (1,0,0,1)
    1 -> (1,1,-1,0)
    2 -> (0,1,-1,-1)
    3 -> (-1,0,0,-1)
    4 -> (-1,-1,1,0)
    5 -> (0,-1,1,1)
  rd = turn (6 - n)

flipBoard :: Board -> Board
flipBoard bd = (M.fromList . map flip1 . M.toList) bd where
  flip1 (t,(p,d)) = (ft t,(p,fd d))
  ft (x,y) = (y-x,y)
  fd = toEnum . (5 -) . fromEnum

startBoard2 = M.union (startp' Blue) (rotateBoard 3 $ startp' Red)
startBoard3 = foldl1 M.union [
  startp' Blue,
  rotateBoard 2 $ startp' Green,
  rotateBoard 4 $ startp' Red
 ]

genMoves p b = nub $ do
  b' <- genMoves' p b
  genMoves' p b'
 where
  genMoves' p b = do
    ((x,y),(p',d)) <- M.toList b
    guard (p == p')
    let
      front = step (x,y) d
      canForward = onBoard front && case M.lookup front b of
        Just (_,d') -> opposing d /= d'
        Nothing -> True
    catMaybes [
      Just (M.insert (x,y) (p,turn 5 d) b),
      if canForward
        then Just (M.delete (x,y) $ M.insert front (p,d) b)
        else Nothing,
      Just (M.insert (x,y) (p,turn 1 d) b)
     ]

whichPlayers :: Board -> [Player]
whichPlayers = whichPlayersFrom Blue

whichPlayersFrom :: Player -> Board -> [Player]
whichPlayersFrom p b = intersect (playersFrom p) $ map fst (M.elems b)

