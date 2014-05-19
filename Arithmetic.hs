{-# LANGUAGE ExistentialQuantification #-}
module Arithmetic (
  Range,
  range,
  DecodeTree,
  (~*~),
  (~/~),
  (~&~),
  (~!~),
  (~>~),
  (~<~),
  (~^~),
  (%),
  runDecode,
  runDecode',
  listDecode,
  modelDecode,
  byteModel,
  fromBytes,
  base64,
  fromBase64,
  hex,
  fromHex,
  decimal,
  fromDecimal
 ) where

import Data.Ratio
import Data.List (foldl1')
import Data.Word
import Debug.Trace

data Range = Range Rational Rational deriving (Eq,Show)

data DecodeTree a =
  DecodeNode Range Rational (() -> DecodeTree a) (() -> DecodeTree a) |
  DecodeLeaf Range a |
  forall b. DecodeLambda (b -> Either (DecodeTree a) a) (DecodeTree b)

range a b = if a <= b then Range a b else Range b a

infixl 7 ~*~
~(Range lo ho) ~*~ ~(Range li hi) = let
  w = ho - lo
  p x = w * x + lo
  in Range (p li) (p hi)

infixl 7 ~/~
~(Range lo ho) ~/~ ~(Range li hi) = let
  w = ho - lo
  p y = (y - lo) / w
  in Range (p li) (p hi)

infixl 6 ~&~
~(Range l1 h1) ~&~ ~(Range l2 h2) = case l1 `compare` l2 of
  LT -> l2 < h1
  EQ -> True
  GT -> l1 < h2

infixl 6 ~!~
~(Range l h) ~!~ v = v >= l && v < h

infixl 6 ~>~
~(Range lo ho) ~>~ x = (lo - ho) * x + lo

infixl 6 ~<~
~(Range lo ho) ~<~ y = (y - lo) / (ho - lo)

infixl 6 ~^~
~(Range lo ho) ~^~ ~(Range li hi) = lo <= li && ho >= hi

instance Functor DecodeTree where
  fmap f t = DecodeLambda (Right . f) t

instance Monad DecodeTree where
  return v = DecodeLeaf (Range 0 1) v
  o >>= f = DecodeLambda (Left . f) o 

decodeStep :: DecodeTree a -> Range -> DecodeTree a
decodeStep (DecodeLeaf n v) t = DecodeLeaf (n ~*~ t) v
decodeStep (DecodeLambda l tr) t = DecodeLambda l (decodeStep tr t)
decodeStep (DecodeNode n s l r) t = let
  i = n ~*~ t
  rl = n ~*~ range 0 s
  rr = n ~*~ range s 1
  in case (i ~&~ rl, i ~&~ rr) of
    (True,True) -> DecodeNode i s l r
    (True,False) -> decodeStep (l ()) (rl ~/~ i)
    (False,True) -> decodeStep (r ()) (rr ~/~ i)
    (False,False) -> error "Unacceptable range appeared"

runDecode :: DecodeTree a -> [Range] -> a
runDecode t rs = let (_,v) = runDecode' False t rs in v

-- I tried to make a monad transformer but it broke laziness. The same end
-- goal can be met by exporting runDecode'
runDecode' :: Bool -> DecodeTree a -> [Range] -> ([Range],a)
runDecode' _ (DecodeLeaf w v) r = (w:r,v)
runDecode' f (DecodeLambda l t) r = let (r',t') = runDecode' f t r in case l t' of
  Left v -> runDecode' f v r'
  Right v -> (r',v)
runDecode' False t@(DecodeNode _ _ _ _) [] =
  ([],runDecode t [Range 0 0])
runDecode' True t@(DecodeNode n s l r) [] = ([],head $ leafList) where
  leafList = ll t
  ll :: DecodeTree a -> [a]
  ll (DecodeLeaf (Range 0 _) v) = [v]
  ll (DecodeLeaf _ _) = []
  ll (DecodeLambda f t) = concatMap be $ ll t where
    be x = case f x of
      Right v -> [v]
      Left v -> ll v
  ll (DecodeNode (Range nl nh) s l r) =
    ll (decodeStep (l ()) (Range 0 s ~/~ Range nl s)) ++
    ll (decodeStep (r ()) (Range s 1 ~/~ Range s nh))
runDecode' f x (r1:rs) = runDecode' f (decodeStep x r1) rs

listDecode :: DecodeTree a -> [Range] -> [a]
listDecode t rs = let
  (r,v) = runDecode' False t rs
  (_,v') = runDecode' True t rs
  in case r of
    [Range 0 _] -> [v']
    [] -> [v']
    _ -> v : listDecode t r

finDecode :: DecodeTree a -> a
finDecode (DecodeNode _ _ l _) = finDecode (l ())
finDecode (DecodeLeaf _ v) = v
finDecode (DecodeLambda l t) = case l (finDecode t) of
  Left t -> finDecode t
  Right v -> v

modelDecode :: Real p => [(p,v)] -> DecodeTree v
modelDecode = ma . map prep where
  prep (p,v) = (toRational p, return v)
  ma [] = error "Cannot build model from empty list!"
  ma [(_,x)] = x
  ma l = ma (mp l)
  mp [] = []
  mp [a] = [a]
  mp ((pa,va):(pb,vb):r) = let
    ps = pa + pb
    pp = pa / ps
    in (ps,DecodeNode (Range 0 1) pp (const va) (const vb)) :
      mp r

byteModel = modelDecode $ zip (repeat 1) [0 :: Word8 .. maxBound]
fromBytes = map be where
  be x = let x' = fromIntegral x in Range (x%256) ((x+1)%256)

decimal = modelDecode $ zip (repeat 1) ['0'..'9']
fromDecimal = map (df . read . (:[])) where
  df x = Range (x%10) ((x+1)%10)

hex = modelDecode $ zip (repeat 1) (['0'..'9']++['A'..'F'])
fromHex = map br where
  br c
   | c >= '0' && c <= '9' = let
     p = read [c]
     in Range (p%16) ((p+1)%16)
   | c >= 'A' && c <= 'F' = let
     p = fromIntegral (fromEnum c - fromEnum 'A' + 10)
     in Range (p%16) ((p+1)%16)
   | c >= 'a' && c <= 'f' = let
     p = fromIntegral (fromEnum c - fromEnum 'a' + 10)
     in Range (p%16) ((p+1)%16)

base64 = modelDecode $ zip (repeat 1) (['A'..'Z']++['a'..'z']++['0'..'9']++"+/")
fromBase64 = map br where
  br c
   | c >= 'A' && c <= 'Z' = let
    p = fromIntegral (fromEnum c - fromEnum 'A')
    in Range (p%64) ((p+1)%64)
   | c >= 'a' && c <= 'z' = let
    p = fromIntegral (fromEnum c - fromEnum 'a' + 26)
    in Range (p%64) ((p+1)%64)
   | c >= '0' && c <= '9' = let
    p = fromIntegral (fromEnum c - fromEnum '0' + 52)
    in Range (p%64) ((p+1)%64)
   | c == '+' = Range (62%64) (63%64)
   | c == '/' = Range (63%64) 1

