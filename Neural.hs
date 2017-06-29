{-# LANGUAGE OverloadedStrings #-}
module Neural (
  NNet,
  Layer(..),
  feedforward,
  backprop,
  backpropSome,
  randomNNet
 ) where

import Data.Array.Unboxed
import Data.List
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Control.DeepSeq
import Control.Parallel.Strategies
import Control.Monad
import System.Random

import ProjectM36.Base (Relation(..))
import ProjectM36.Client
import ProjectM36.Relation
import ProjectM36.Tuple

newtype Layer = Layer (UArray (Int,Int) Double) deriving (Eq)
type NNet = [Layer]

instance Read Layer where
  readsPrec p s = map (\ ~(a,r) -> let x = ul a in x `deepseq` (x, r)) $
    readsPrec p s
   where
    ul l = let
      w = minimum (map length l) - 1
      h = length l - 1
      in Layer $ array ((0,0),(w,h)) $ do
        (y,r) <- zip [0 .. h] l
        (x,v) <- zip [0 .. w] r
        return ((x,y),v)

instance Show Layer where
  showsPrec p (Layer l) = let
    ((x0,y0),(xn,yn)) = bounds l
    in showsPrec p $ map (\y -> map (\x -> l ! (x,y)) [x0 .. xn]) [y0 .. yn]

synapse_attrs = attributesFromList [
  Attribute "inputIndex" IntAtomType,
  Attribute "outputIndex" IntAtomType,
  Attribute "weight" DoubleAtomType
 ]

instance Binary Layer where
  put (Layer l) = do
    let b@((x0,y0),(xn,yn)) = bounds l
    put (xn - x0)
    put (yn - y0)
    forM_ (range b) $ \(x,y) -> put (l ! (x,y))
  get = do
    w <- get
    h <- get
    let b = ((0,0),(w,h))
    fmap (Layer . array b) $ forM (range b) $ \(x,y) -> fmap ((,) (x,y)) get

instance Atomable Layer where
  toAtom (Layer l) = let
    Right r = mkRelationFromList synapse_attrs $ do
      (x,y) <- range $ bounds l
      return [toAtom x, toAtom y, toAtom (l ! (x,y))]
    in toAtom r
  fromAtom a = let
    r :: Relation
    r = fromAtom a
    mg _ a Nothing = a
    mg _ Nothing b = b
    mg f (Just a) (Just b) = Just (f a b)
    e2m (Left _) = Nothing
    e2m (Right r) = Just r
    (Just x0, Just xn, Just y0, Just yn) = relFold (\t (x0',xn',y0',yn') -> let
      [x,y] = map (\n -> fmap fromAtom $ e2m (atomForAttributeName n t))
        ["inputIndex","outputIndex"]
      in force (mg min x0' x, mg max xn' x, mg min y0' y, mg max yn' y)
     ) (Nothing,Nothing,Nothing,Nothing) r
    in Layer $ array ((x0,y0),(xn,yn)) $ relFold (\t r' -> let
      Right e = do
        x <- atomForAttributeName "inputIndex" t
        y <- atomForAttributeName "outputIndex" t
        w <- atomForAttributeName "weight" t
        return ((fromAtom x, fromAtom y), fromAtom w)
      in e : r'
     ) [] r
  toAtomType _ = RelationAtomType synapse_attrs
  toDatabaseContextExpr _ = NoOperation

instance NFData Layer where
  rnf (Layer l) = l `seq` ()

sigmoid :: Double -> Double
sigmoid t = 1 / (1 + exp (-t))

-- This is the derivative given the sigmoid value.
-- For the derivative given x: use sigmoidDerivative . sigmoid
sigmoidDerivative :: Double -> Double
sigmoidDerivative fx = fx * (1 - fx)

feedforward :: NNet -> [Double] -> [Double]
feedforward = flip (foldl' feedlayer)

feedlayer :: [Double] -> Layer -> [Double]
feedlayer i (Layer l) = let
  ((x0,y0),(xn,yn)) = bounds l
  in withStrategy (parList rdeepseq) $ map
    (\y -> sigmoid $ sum $ zipWith (\x i' -> (l ! (x,y)) * i') [x0 .. xn] (1:i))
    [y0 .. yn]

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
backprop rate i t = backpropSome rate i (map Just t)

backpropSome :: Double -> [Double] -> [Maybe Double] -> NNet -> NNet
backpropSome rate i t n = fst $ backprop' i t n where
  backprop' i t (l@(Layer la):n) = (nw:r,be) where
    ab@((x0,y0),(xn,yn)) = bounds la
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
    we :: UArray (Int,Int) Double
    we = array ab $ withStrategy (parList rdeepseq) $ do
      (oe,y) <- zip e [y0 .. yn]
      x <- [x0 .. xn]
      return ((x,y), oe * (la ! (x,y)))
      
    -- nw: New weights for this layer
    -- wl: weights leading to current node
    -- h: this node's output
    nw = Layer $ array ab $ withStrategy (parList rdeepseq) $ do
      (y,d,h) <- zip3 [y0 .. yn] e hs
      let sdh = sigmoidDerivative h
      (x,i') <- zip [x0 .. xn] (1 : i)
      return ((x,y), (la ! (x,y)) + rate * d * sdh * i')
    -- be: Errors to propagate back to earlier nodes
    be = map (\x -> sum $ map (\y -> we ! (x,y)) [y0 .. yn]) [x0 .. xn]

randomNNet :: RandomGen g => g -> [Int] -> NNet
randomNNet _ [_] = []
randomNNet gen (i:r@(n:_)) = let
  i' = i + 1
  (gen',l1) =
    mapAccumL (const . uncurry (flip (,)) . randomR (-0.05, 0.05)) gen $
    replicate ((i + 1) * n) ()
  ar = ((0,0),(i, n - 1))
  l1' = Layer $ array ar $ zip (range ar) l1
  in l1' : randomNNet gen' r

