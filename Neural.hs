module Neural (
  NNet,
  Layer,
  feedforward,
  backprop
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
    hs = feedlayer i l
    (r,e) = case n of
      [] -> ([], zipWith subtract hs t)
      x -> backprop' hs t n
    we = zipWith (\oe w ->
      map (*oe) w
     ) e l
    nw = zipWith3 (\wl dl h -> let sdh = sigmoidDerivative h in
      zipWith3 (\w d x ->
        w + rate * d * sdh * x
       ) wl dl (1:i)
     ) l we hs
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

