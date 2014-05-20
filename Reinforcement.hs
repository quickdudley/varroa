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
  outputPairs' = case length playing of
    2 -> map (\(_,v) -> (1,v)) outputPairs
    3 -> outputPairs
    1 -> [(1,1)]
  in M.union (M.fromList $ zip playing outputPairs') everyoneLost

isomorphisms b = nub $ do
  b' <- [b,flipBoard b]
  b' : map (flip rotateBoard b') [1..5]

updateENet :: Player -> Board -> Board -> NNet -> NNet
updateENet p bo bt n = let
  playing = whichPlayersFrom p bo
  nextPlayer = (cycle playing) !! 1
  er = evaluate n nextPlayer bt
  mo = case length playing of
    3 -> map Just $ concatMap ((\(a,b) -> [a,b]) . (er M.!)) playing
    2 -> take 3 $
      concatMap ((\(_,b) -> [Nothing, Just b]) . (er M.!)) playing ++
      repeat Nothing
  ufs = map (\b ->
    backpropSome 0.1 (mapBoard p b) mo
   ) $ isomorphisms bo
  in foldl1' (.) ufs n

selectMove :: (Monad m) => Double -> NNet -> Player -> Board ->
  DecodeT m Board
selectMove s n p b = let
  playing = whichPlayersFrom p b
  nextPlayer = (cycle playing) !! 1
  sf = case length playing of
    2 -> flip const
    3 -> \b a -> b*s + a
  in transformDecoder $ modelDecode $ map (\b' ->
    (uncurry sf (evaluate n nextPlayer b' M.! p),b')
   ) $ genMoves p b

