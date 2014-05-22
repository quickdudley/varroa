{-# LANGUAGE ExistentialQuantification #-}
module Arithmetic (
  Range,
  range,
  DecodeTree,
  DecodeT,
  (~*~),
  (~/~),
  (~&~),
  (~!~),
  (~>~),
  (~<~),
  (~^~),
  (%),
  runDecode,
  runDecodeT,
  transformDecoder,
  listDecode,
  modelDecode,
  randomA,
  byteModel,
  fromBytes,
  base64,
  fromBase64,
  hex,
  fromHex,
  decimal,
  fromDecimal
 ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Morph
import Data.Ratio
import Data.List (foldl1')
import Data.Word
import System.Random

data Range = Range !Rational !Rational deriving (Eq,Show)

--Wishlist: Refactor DecodeTree to "type DecodeTree = DecodeT Identity"
--without losing listDecode.

data DecodeTree a =
  DecodeNode Range Rational (() -> DecodeTree a) (() -> DecodeTree a) |
  DecodeLeaf Range a |
  forall b. DecodeLambda (b -> Either (DecodeTree a) a) (DecodeTree b)

data DecodeT m a =
  DecodeNodeT Range Rational (DecodeT m a) (DecodeT m a) |
  DecodeLeafT Range a |
  forall b. DecodeLambdaT (b -> Either (m (DecodeT m a)) a) (DecodeT m b)

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

instance Applicative DecodeTree where
  pure = return
  (<*>) = ap

-- Monad laws preserved thanks to the semantics of DecodeLambda. Would be
-- broken if functions from outside this module were allowed to pattern-match.
instance Monad DecodeTree where
  return v = DecodeLeaf (Range 0 1) v
  o >>= f = DecodeLambda (Left . f) o

instance (Functor m) => Functor (DecodeT m) where
  fmap f t = DecodeLambdaT (Right . f) t

instance (Functor m, Monad m) => Applicative (DecodeT m) where
  pure = return
  (<*>) = ap

instance (Monad m) => Monad (DecodeT m) where
  return = DecodeLeafT (Range 0 1)
  o >>= f = DecodeLambdaT (Left . \i -> return (f i)) o

-- MonadPlus is the one instance I can't figure out, but I think I can make
-- do without it for now. Any future contributor is welcome to have a try.

instance MonadTrans DecodeT where
  lift m = DecodeLambdaT (Left . \_ -> liftM return m) (return ())

instance MFunctor DecodeT where
  hoist _ (DecodeLeafT r a) = DecodeLeafT r a
  hoist f (DecodeNodeT n s l r) = DecodeNodeT n s (hoist f l) (hoist f r)
  hoist f (DecodeLambdaT l t) = DecodeLambdaT (\v -> case l v of
    Left x -> Left $ f $ liftM (hoist f) x
    Right x -> Right x
   ) (hoist f t)

instance (MonadIO m) => MonadIO (DecodeT m) where
  liftIO = lift . liftIO

transformDecoder :: Monad m => DecodeTree a -> DecodeT m a
transformDecoder (DecodeLeaf n v) = DecodeLeafT n v
transformDecoder (DecodeNode n s l r) = DecodeNodeT n s 
  (transformDecoder (l ())) (transformDecoder (r ()))
transformDecoder (DecodeLambda l tr) = DecodeLambdaT
  (\i -> case l i of
      Left v -> (Left . return . transformDecoder) v
      Right v -> Right v)
  $ transformDecoder tr

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

decodeStepT :: DecodeT m a -> Range -> DecodeT m a
decodeStepT (DecodeLeafT n v) t = DecodeLeafT (n ~*~ t) v
decodeStepT (DecodeLambdaT l tr) t = DecodeLambdaT l (decodeStepT tr t)
decodeStepT (DecodeNodeT n s l r) t = let
  i = n ~*~ t
  rl = n ~*~ range 0 s
  rr = n ~*~ range s 1
  in case (i ~&~ rl, i ~&~ rr) of
    (True,True) -> DecodeNodeT i s l r
    (True,False) -> decodeStepT l (rl ~/~ i)
    (False,True) -> decodeStepT r (rr ~/~ i)
    (False,False) -> error "Unacceptable range appeared"

runDecodeT :: (Monad m) => DecodeT m a -> [Range] -> m a
runDecodeT t r = liftM snd $ runDecodeT' t r

runDecode :: DecodeTree a -> [Range] -> a
runDecode t rs = let (_,v) = runDecode' False t rs in v

runDecode' :: Bool -> DecodeTree a -> [Range] -> ([Range],a)
runDecode' _ (DecodeLeaf w v) r = let
  r' = if w == Range 0 1 then r else w:r
  in (r',v)
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

runDecodeT' :: Monad m => DecodeT m a -> [Range] -> m ([Range],a)
runDecodeT' (DecodeLeafT w v) r = let
  r' = if w == Range 0 1 then r else w:r
  in return (r',v)
runDecodeT' (DecodeLambdaT l t) r = do
  ~(r',t') <- runDecodeT' t r
  case l t' of
    Left v -> do
      v' <- v
      runDecodeT' v' r'
    Right v -> return (r',v)
runDecodeT' t@(DecodeNodeT _ _ _ _) [] = do
  r <- runDecodeT t [Range 0 0]
  return ([],r)
runDecodeT' x (r1:rs) = runDecodeT' (decodeStepT x r1) rs

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
modelDecode = (ma . map prep . filter ((> 0) . fst)) where
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

randomA :: (RandomGen g) => g -> [Range]
randomA = fromBytes . randoms

byteModel = modelDecode $ zip (repeat 1) [0 :: Word8 .. maxBound]
fromBytes :: [Word8] -> [Range]
fromBytes = map be where
  be x = let x' = fromIntegral x in Range (x'%256) ((x'+1)%256)

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

