import Control.Monad
import Control.Monad.Identity
import Control.Monad.Writer
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import System.Directory (doesFileExist,renameFile)
import System.IO
import System.Random

import Arithmetic
import Board
import Neural
import Reinforcement

data VLS = VLS {
  teacher :: NNet,
  student :: NNet,
  lead :: Int
 } deriving (Read,Show)

main = do
  c <- doesFileExist "brain"
  vls <- if c
    then liftM read (readFile "brain")
    else liftM initialVLS getStdGen
  foldr1 (>=>) (repeat train1game) vls

initialVLS g = let
  n = randomNNet g [1638,60,6]
  in VLS {
    teacher = n,
    student = n,
    lead = 0
   }

train1game vls' = do
  let
    vls = if lead vls' < 5
      then vls'
      else vls' {teacher = student vls', lead = 0}
    td = selfTrain (teacher vls) (student vls)
  rs <- liftM randomA newStdGen
  let
    gr = appEndo (runIdentity $ execWriterT $ runDecodeT td rs) []
    (p,_) = head gr
    sp = head $ M.keys $ M.filter isStudent p
  putStrLn ((show $ M.size p) ++ " player game. " ++ show sp ++ " is student.")
  commentary vls gr

commentary vls [] = return vls
commentary vls [(p,b)] = do
  let w = head $ whichPlayers b
  putStrLn $ "\n" ++ show w ++ " wins."
  let sp = head $ M.keys $ M.filter isStudent p
  return $ vls {lead = (if sp == w then (+) else (-)) (lead vls) 1}
commentary vls ((_,b'):r@((p,b):_)) = do
  let vls' = updateVLS p vls
  saveVLS vls' "brain"
  putStr "."
  hFlush stdout
  let elim = whichPlayers b' \\ whichPlayers b
  sequence $ map (\e -> putStrLn $ "\n" ++ show e ++ "Has lost.") elim
  vls' `seq` commentary vls' r

updateVLS p vls = maybe vls (\n -> vls {student = n})
  (actorNNet $ head $ filter isStudent $ M.elems p)

saveVLS vls fn = do
  let tfn = (fn ++ "~")
  h <- openFile tfn WriteMode
  hPutStr h $ show vls
  hClose h
  renameFile tfn fn

