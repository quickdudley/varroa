module Reinforcement (
 ) where

import Board
import Neural

import Data.List
import qualified Data.Map as M

playersFrom Blue = [Blue,Green,Red]
playersFrom Green = [Green,Red,Blue]
playersFrom Red = [Red,Blue,Green]

{-
Represent the board as a list of doubles for feeding to the neural network.
Explanation of magic numbers:
  A tile can face 6 directions, there are up to 3 players in the game,
  6*3 = 18.
  So each space on the grid gets 18 inputs, only 1 of which will be set to 1
  at any given time.
The total number of inputs is 1638.
-}
mapBoard :: Player -> Board -> [Double]
mapBoard p b = concatMap mapTile boardRange where
  mapTile l = case M.lookup l b of
    Nothing -> replicate 18 0
    Just (p,d) -> let
      tn = tileN p d
      in replicate tn 0 ++ [1] ++ replicate (17 - tn) 0
  playing = intersect (playersFrom p) $ map fst (M.elems b)
  pns = M.fromList $ zip playing [0,6..]
  tileN p d = pns M.! p + fromEnum d

