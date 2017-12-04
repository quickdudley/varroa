{-# LANGUAGE DataKinds #-}
module Reinforcement (
  Actor
 ) where

import Arithmetic (
  Range,
  fromBytes,
  DecodeT(..),
  runDecode,
  runDecodeT,
  randomA,
  modelDecode,
  truncate
 )
import Board

import AI.HFNN
import Control.Monad.Identity
import Control.Monad.Morph (hoist)
import Control.Monad.Writer
import Data.Int
import Data.List
import Data.Monoid
import qualified Data.Map as M
import System.Random

data Actor m =
  Teacher Double (NNStructure False) WeightValues |
  Student Double (NNStructure False) WeightValues |
  Outsider (Player -> Board -> m Board) (Player -> Board -> m ())

actorSolidarity :: Actor m -> Double
actorSolidarity (Teacher s _ _) = s
actorSolidarity (Student s _ _) = s
actorSolidarity _ = 0

isStudent (Student _ _ _) = True
isStudent _ = False

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

standardStructure :: NNStructure False
standardStructure = fst $ runNNBuilder $ do
  i <- fmap M.fromList $ forM boardRange $ \k -> ((,) k) <$> addInputs 18
  let
    mkLayer :: M.Map (Int8,Int8) (Layer s) ->
      NNBuilder False s (M.Map (Int8,Int8) (Layer s))
    mkLayer a = undefined
  undefined

evaluate :: NNStructure False -> WeightValues ->
  Player -> Board -> M.Map Player (Double,Double)
evaluate nn p b = undefined

isomorphisms b = nub $ do
  b' <- [b,flipBoard b]
  b' : map (`rotateBoard` b') [1..5]
