module Neural (
 ) where

import Data.List

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
backprop rate i e n = fst $ backprop' i e n where
  backprop' i ef [] = ([],zipWith subtract i ef)
  backprop' i ef (l:nr) = let
    hs = feedlayer i l
    (nr',ec) = backprop' (hs) ef nr
    em = zipWith bem l ec
    bem ws err = zipWith (*) i $ map (* err) ws
    nes = map sum em
    updateNode ws err = zipWith3 (updateWeight err) ws i hs
    updateWeight err w a h = w + rate * err * h * a
    in (zipWith updateNode l nes : nr',map sum $ transpose em)

