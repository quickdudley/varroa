module Reinforcement (
  Actor,
  isStudent,
  actorNNet,
  selfTrain
 ) where

import Arithmetic (
  Range,
  fromBytes,
  DecodeTree,
  DecodeT(..),
  runDecode,
  runDecodeT,
  randomA,
  transformDecoder,
  modelDecode
 )
import Board
import Neural

import Control.Monad.Identity
import Control.Monad.Morph (hoist)
import Control.Monad.Writer
import Data.List
import Data.Monoid
import qualified Data.Map as M
import System.Random

data Actor m =
  Teacher Double NNet |
  Student Double NNet |
  Outsider (Player -> Board -> m Board) (Player -> Board -> m ())

actorNNet :: Actor m -> Maybe NNet
actorNNet (Teacher _ n) = Just n
actorNNet (Student _ n) = Just n
actorNNet _ = Nothing

actorSolidarity :: Actor m -> Double
actorSolidarity (Teacher s _) = s
actorSolidarity (Student s _) = s
actorSolidarity _ = 0

isStudent (Student _ _) = True
isStudent _ = False

-- Note: the first argument of this function represents the player
-- that just moved. The Board arguments are the previous and current
-- boards.
actorNotify :: (Monad m) =>
  Player -> Board -> Board -> Actor m -> m (Actor m)
actorNotify p b1 b2 (Student c n) = return $ Student c $ updateENet p b1 b2 n
actorNotify _ _ _ t@(Teacher _ _) = return t
actorNotify p b1 b2 o@(Outsider _ nf) = do
  nf p b2
  return o

actorMove :: (Monad m) => Actor m -> Player -> Board -> DecodeT m Board
actorMove a p b = case actorNNet a of
  Just nn -> selectMove (actorSolidarity a) nn p b
  Nothing -> let
    Outsider mf _ = a
    in lift $ mf p b

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

evaluate :: NNet -> Player -> Board -> M.Map Player (Double,Double)
evaluate nn p b = let
  playing = whichPlayersFrom p b
  everyoneLost = M.fromList $ zip [Red .. Blue]  $ repeat (0,0)
  nnInputs = mapBoard p b
  nnOutputs = feedforward nn nnInputs
  outputPairs = op nnOutputs where
    op (a:b:r) = (a,b) : op r
    op _ = []
  outputPairs' = case length playing of
    2 -> map (\(_,v) -> (1,v)) outputPairs
    3 -> outputPairs
    1 -> [(1,1)]
  in M.union (M.fromList $ zip playing outputPairs') everyoneLost

isomorphisms b = nub $ do
  b' <- [b,flipBoard b]
  b' : map (flip rotateBoard b') [1..5]

updateENet :: Player -> Board -> Board -> NNet -> NNet
updateENet p bo bt n = let
  playing = whichPlayersFrom p bo
  nextPlayer = (cycle playing) !! 1
  er = evaluate n nextPlayer bt
  mo = case length playing of
    3 -> map Just $ concatMap ((\(a,b) -> [a,b]) . (er M.!)) playing
    2 -> take 3 $
      concatMap ((\(_,b) -> [Nothing, Just b]) . (er M.!)) playing ++
      repeat Nothing
  ufs = map (\b ->
    backpropSome 0.1 (mapBoard p b) mo
   ) $ isomorphisms bo
  in foldl1' (.) ufs n

selectMove :: (Monad m) => Double -> NNet -> Player -> Board ->
  DecodeT m Board
selectMove s n p b = let
  playing = whichPlayersFrom p b
  nextPlayer = (cycle playing) !! 1
  sf = case length playing of
    2 -> flip const
    3 -> \b a -> b*s + a
  in transformDecoder $ modelDecode $ map (\b' ->
    (uncurry sf (evaluate n nextPlayer b' M.! p),b')
   ) $ genMoves p b

-- I am using a transformer stack, yet I'm manually passing state. Yes I know.
continueGame :: (Monad m) =>
  Player ->
  Board ->
  M.Map Player (Actor m) ->
  DecodeT (WriterT (Endo [(M.Map Player (Actor m),Board)]) m) Board
continueGame cp' b a = do
  let
    playing = whichPlayersFrom cp' b
    cp = head playing
  if null $ tail playing
    then do
      lift $ tell $ Endo ([(a,b)]++)
      return b
    else do
      let ca = (a M.! cp)
      move <- hoist lift $ actorMove ca cp b
      a' <- lift $ lift $ liftM M.fromList $ sequence $ map (\(p1,a1) -> do
        a2 <- actorNotify cp b move a1
        return (p1,a2)
       ) $ M.toList a
      lift $ tell $ Endo ([(a',move)]++)
      continueGame (playing !! 1) move a'

selfTrain :: NNet -> NNet
  -> DecodeT
    (WriterT (Endo [(M.Map Player (Actor Identity),Board)]) Identity)
    Board
selfTrain t s = do
  ~(start,players) <- transformDecoder $ modelDecode
    [(1,(startBoard2,[Blue,Red])),(1,(startBoard3,[Blue,Green,Red]))]
  student <- transformDecoder $ modelDecode $
    map (\x -> (1,x)) players
  let
    teacherA = Teacher 0.5 t
    studentA = Student 0.5 s
    actors = M.insert student studentA $ M.fromList $ map
      (\p -> (p,teacherA))
      players
  lift $ tell $ Endo ([(actors,start)]++)
  continueGame Blue start actors

