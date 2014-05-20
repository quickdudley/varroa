module Neural (
  NNet,
  Layer,
  feedforward,
  backprop,
  backpropSome,
  randomNNet
 ) where

import Data.List
import System.Random

type Layer = [[Double]]
type NNet = [Layer]

sigmoid :: Double -> Double
sigmoid t = 1 / (1 + exp (-t))

-- This is the derivative given the sigmoid value.
-- For the derivative given x: use sigmoidDerivative . sigmoid
sigmoidDerivative :: Double -> Double
sigmoidDerivative fx = fx * (1 - fx)

feedforward = flip (foldl' feedlayer)

feedlayer i = map (sigmoid . sum . zipWith (*) (1:i))

{-
What was causing the bug
========================
When an error signal reaches a node, this should happen:
      -->Multiply by weights-->Propagate to earlier nodes
     /
error
     \
      -->Update weights.

What was happening before I fixed the bug:
                            -->Propagate to earlier nodes
                           /
error-->Multiply by weights
                           \
                            -->Update weights
-}
backprop :: Double -> [Double] -> [Double] -> NNet -> NNet
backprop rate i t n = backpropSome rate i (map Just t) n

backpropSome :: Double -> [Double] -> [Maybe Double] -> NNet -> NNet
backpropSome rate i t n = fst $ backprop' i t n where
  backprop' i t (l:n) = (nw:r,be) where
    -- hs: output of this layer
    hs = feedlayer i l
    -- r: the next layer updated
    -- e: the error of this layer's output
    (r,e) = case n of
      [] -> ([], zipWith (\o m -> case m of
        Nothing -> 0
        Just v -> v - o
       ) hs t)
      x -> backprop' hs t n
    -- we: Error divided among weights
    we = zipWith (\oe w ->
      map (*oe) w
     ) e l
    -- nw: New weights for this layer
    -- wl: weights leading to current node
    -- h: this node's output
    nw = zipWith3 (\wl d h -> let sdh = sigmoidDerivative h in
      -- w: The current weight
      -- d: The error assigned to the current node
      -- x: The input to the current synapse
      zipWith (\w x ->
        w + rate * d * sdh * x
       ) wl (1:i)
     ) l e hs
    -- be: Errors to propagate back to earlier nodes
    be = map sum $ transpose we

randomNNet :: RandomGen g => g -> [Int] -> NNet
randomNNet _ [_] = []
randomNNet gen (i:r@(n:_)) = let
  frl g _ = mapAccumL (\g _ -> let
    (a,g') = randomR (-0.05,0.05) g
    in (g',a)) g $ replicate (i+1) ()
  frg g = mapAccumL frl g $ replicate n ()
  (gen',l1) = frg gen
  in l1 : randomNNet gen' r

