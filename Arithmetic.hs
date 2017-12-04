{-# Language RankNTypes #-}
module Arithmetic (
  Range,
  range,
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
  Arithmetic.truncate,
  listDecode,
  modelDecode,
  randomA,
  byteModel,
  fromBytes
 ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Morph
import Control.Monad.Writer
import Data.Functor.Identity
import Data.Monoid (Endo(..))
import Data.Ratio
import Data.List (foldl1',sum)
import Data.Word
import System.Random

data Range = Range Rational Rational deriving (Eq,Show)

data Decoder m a =
  Action (m (Decoder m a)) |
  Consume (Range -> Decoder m a) |
  Result a

newtype DecodeT m a =
  D {cpsDecoder :: forall b . (a -> Decoder m b) -> Decoder m b}

type Decode = DecodeT Identity

range a b = if a <= b then Range a b else Range b a

instance Functor (DecodeT m) where
  fmap f (D d) = D (d . (. f))

instance Applicative (DecodeT m) where
  pure a = D ($ a)
  D f <*> D a = D (f . (a .) . (.))
  D a *> D b = D (a . const . b)
  D a <* D b = D (a . ((b . const) .))

instance Monad (DecodeT m) where
  return = pure
  (>>) = (*>)
  D a >>= f = D (a . flip (cpsDecoder . f))

instance MonadTrans DecodeT where
  lift a = D (Action . flip fmap a)

instance MFunctor DecodeT where
  hoist f (D d) = D (\c -> let
    go (Action a) = Action (f (fmap go a))
    go (Consume d) = Consume (fmap go d)
    go (Result a) = c a
    in go (d Result)
   )

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

runDecodeT :: Monad m => DecodeT m a -> [Range] -> m a
runDecodeT (D d) = runDecoder (d Result)

runDecode :: Decode a -> [Range] -> a
runDecode = (runIdentity .) . runDecodeT

runDecoder :: Monad m => Decoder m a -> [Range] -> m a
runDecoder = go where
  go (Result a) _ = return a
  go (Consume d) (a:r) = go (d a) r
  go (Consume d) [] = go (d (Range 0 0)) []
  go (Action a) r = a >>= flip go r

idleDecoder :: Monad m => Decoder m a -> m (Decoder m a)
idleDecoder (Action a) = a >>= idleDecoder
idleDecoder d = return d

stepDecoder :: Functor m => Decoder m a -> Range -> Decoder m a
stepDecoder d' r = go d' where
  go d@(Result _) = d
  go (Consume d) = d r
  go (Action a) = Action (fmap go a)

truncate :: Functor m => (Rational -> Rational) -> DecodeT m ()
truncate f = D (\c ->
  Consume (\ ~(Range l h) -> stepDecoder (c ()) (Range (f l) (f h)))
 )

modelDecode :: (Functor m, Real p) => [(p,v)] -> DecodeT m v
modelDecode l = D (\c -> let
  go rl sr = case ds rl sr of
    [(nr,v)]
      | nr == sr -> c v
      | otherwise -> stepDecoder (c v) (nr ~/~ sr)
    rl' -> Consume (\sr' -> go rl' (sr ~*~ sr'))
  in go (build rl 0) (Range 0 1)
 ) where
  rl = map (\(p,v) -> (toRational p, v)) l
  total = sum $ map fst rl
  build [] _ = []
  build ((0,_):r) b = build r b
  build ((p,v):r) b = let
    p' = p / total
    b' = b + p'
    in (Range b b', v) : build r b'
  ds [] _ = []
  ds (i@(Range ol oh,_):r) g@(Range l h)
    | ol >= h = []
    | oh < l = ds r g
    | otherwise = i : ds r g

modelIntegral :: (Functor m, Integral v) => v -> v -> DecodeT m v
modelIntegral l h = D (\c -> let
  go r@(Range cl ch) = let
    ccl = floor cl
    m1 x = let
      n = numerator x
      d = denominator x
      in (n `mod` d) % d
    in if ch <= (toRational ccl + 1)
      then if denominator cl == 1 && denominator cl == 1
        then c ccl
        else stepDecoder (c ccl) (range (m1 cl) (case m1 ch of {0 -> 1; z -> z}))
      else Consume (\r2 -> go (r ~*~ r2))
  in go $ range (toRational l) (toRational h + 1)
 )

listDecodeT :: Monad m => DecodeT m v -> [Range] -> m [v]
listDecodeT d = let
  e = D (\c -> Consume (\r -> case r of
    Range 0 0 -> c False
    _ -> stepDecoder (c True) r
   ))
  d' = hoist lift d
  go = e >>= \cc -> when cc $ d' >>= \ v -> lift (tell (Endo (v :))) >> go
  in fmap (($ []) . appEndo) . execWriterT . runDecodeT go

listDecode :: Decode v -> [Range] -> [v]
listDecode = (runIdentity .) . listDecodeT

randomA :: (RandomGen g) => g -> [Range]
randomA g = let
  (a,b) = genRange g
  (m,g') = next g
  w = fromIntegral (b - a)
  l = fromIntegral (m - a)
  in Range (l % w) ((l + 1) % w) : randomA g'

byteModel :: Functor m => DecodeT m Word8
byteModel = modelIntegral minBound maxBound
fromBytes :: [Word8] -> [Range]
fromBytes = map be where
  be x = let x' = fromIntegral x in Range (fromRational $ x'%256) (fromRational $ (x'+1)%256)

