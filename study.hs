import Control.Monad
import Control.Monad.Identity
import Control.Monad.Writer
import qualified Data.Map as M
import Data.Monoid
import System.Random

import Arithmetic
import Neural
import Reinforcement

data VLS = VLS {
  teacher :: NNet,
  student :: NNet,
  lead :: Int
 } deriving (Read,Show)

initialVLS g = let
  n = randomNNet g [1638,60,6]
  in VLS {
    teacher = n,
    student = n,
    lead = 0
   }

train1game vls = do
  let td = selfTrain (teacher vls) (student vls)
  rs <- liftM randomA newStdGen
  let
    gr = appEndo (runIdentity $ execWriterT $ runDecodeT td rs) []
    (p,_) = head gr
    sp = head $ M.keys $ M.filter isStudent p
  putStrLn ((show $ M.size p) ++ " player game. " ++ show sp ++ " is student.")

