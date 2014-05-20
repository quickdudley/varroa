module Reinforcement (
 ) where

import Arithmetic (
  Range,
  fromBytes,
  DecodeTree,
  DecodeT(..),
  runDecode,
  runDecodeT,
  randomA,
  transformDecoder,
  modelDecode
 )
import Board
import Neural

import Data.List
import qualified Data.Map as M
import System.Random

{-
Represent the board as a list of doubles for feeding to the neural network.
Explanation of magic numbers:
  A tile can face 6 directions, there are up to 3 players in the game,
  6*3 = 18.
  So each space on the grid gets 18 inputs, only 1 of which will be set to 1
  at any given time.
There are 91 spaces on the grid, so the total number of inputs is 1638.
-}
mapBoard :: Player -> Board -> [Double]
mapBoard p b = concatMap mapTile boardRange where
  mapTile l = case M.lookup l b of
    Nothing -> replicate 18 0
    Just (p,d) -> let
      tn = tileN p d
      in replicate tn 0 ++ [1] ++ replicate (17 - tn) 0
  playing = whichPlayersFrom p b
  pns = M.fromList $ zip playing [0,6..]
  tileN p d = pns M.! p + fromEnum d

evaluate :: NNet -> Player -> Board -> M.Map Player (Double,Double)
evaluate nn p b = let
  playing = whichPlayersFrom p b
  everyoneLost = M.fromList $ zip [Red .. Blue]  $ repeat (0,0)
  nnInputs = mapBoard p b
  nnOutputs = feedforward nn nnInputs
  outputPairs = op nnOutputs where
    op (a:b:r) = (a,b) : op r
    op _ = []
  in M.union (M.fromList $ zip playing outputPairs) everyoneLost

