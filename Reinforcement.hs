{-# LANGUAGE DataKinds #-}
module Reinforcement (

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
import Control.Concurrent.STM
import Data.List
import Data.Monoid
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
  pns = M.fromList $ zip (playersFrom p) [0,6..]
  tileN p d = pns M.! p + fromEnum d

standardStructure :: NNStructure False
standardStructure = fst $ runNNBuilder $ do
  i <- fmap M.fromList $ forM boardRange $ \k -> ((,) k) <$> addInputs 18
  let
    mkLayer :: M.Map (Int8,Int8) (Layer s) ->
      NNBuilder False s (M.Map (Int8,Int8) (Layer s))
    mkLayer a = do
      let
        s1 = let (_, l) = M.findMin a in layerSize l
        s2 = s1 + 8
      b <- addBaseWeights 1 s2
      cw <- addBaseWeights s1 s2
      sw <- fmap M.fromList $ forM [NE .. NW] $ \d -> ((,) d) <$>
        addBaseWeights s1 s2
      let
        sc c = (((a M.! c,cw):)<$>) $ forM [NE .. NW] $ \d -> let
          c' = step c d
          in (,) <$> M.lookup c' a <*> M.lookup d sw
      fmap (foldr M.union M.empty) $ forM (M.keys a) $ \c -> case sc c of
        Just l' -> do
          Just nl <- standardLayer ((bias,b):l') ahsin
          return (M.singleton c nl)
        Nothing -> return M.empty
    stackc a = case M.size a of
      1 -> return $ snd $ M.findMin a
      _ -> mkLayer a >>= stackc
  fl <- stackc i
  let fls = layerSize fl
  ob <- addBaseWeights 1 6
  ow <- addBaseWeights fls 6
  Just o <- standardLayer [(bias,ob),(fl,ow)] logistic
  addOutputs o

evaluate :: NNStructure False -> WeightValues ->
  Player -> Board -> (FeedForward False, M.Map Player (Double,Double))
evaluate ns wv p b = let
  ff = feedForward ns wv $ mapBoard p b
  pl = playersFrom p
  oi = [(0,1),(2,3),(4,5)]
  m = M.fromList $
    zipWith (\p' (i1,i2) -> (p,(getOutput ff i1, getOutput ff i2))) pl oi
  in (ff,m)

isomorphisms b = nub $ do
  b' <- [b,flipBoard b]
  b' : map (`rotateBoard` b') [1..5]
