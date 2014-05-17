module Neural (
  NNet,
  Layer,
  feedforward,
  backprop,
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

backprop :: Double -> [Double] -> [Double] -> NNet -> NNet
backprop rate i t n = fst $ backprop' i t n where
  backprop' i t (l:n) = (nw:r,be) where
    -- hs: output of this layer
    hs = feedlayer i l
    -- r: the next layer updated
    -- e: the error of this layer's output
    (r,e) = case n of
      [] -> ([], zipWith subtract hs t)
      x -> backprop' hs t n
    -- we: Error divided among weights - possibly buggy
    we = zipWith (\oe w ->
      map (*oe) w
     ) e l
    -- nw: New weights for this layer - possibly buggy
    -- wl: weights leading to current node
    -- dl: error by weight for current node
    -- h: this node's output
    nw = zipWith3 (\wl dl h -> let sdh = sigmoidDerivative h in
      -- w: The current weight
      -- d: The error assigned to the current weight
      -- x: The input to the current synapse
      zipWith3 (\w d x -> let
        delta' = rate * d * sdh * x
        delta = if abs w < 1.0e-80
          then 1.1e-80 * (if delta' < 0 then -1 else 1)
          else delta'
        in w + delta
       ) wl dl (1:i)
     ) l we hs
    -- be: Errors to propagate back to posterior nodes
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

