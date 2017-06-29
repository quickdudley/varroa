import Data.List
import System.Random
import System.IO
import Neural

learningrate = 0.16

main = do
  g <- getStdGen
  csv <- openFile "learning.dat" WriteMode
  let un = randomNNet g [2,4,1]
  let
   uloop n = do
     let r = concatMap (feedforward n) [[0,0],[1,1],[1,0],[0,1]]
     hPutStrLn csv $ intercalate " " $ map show r
     case map (\n -> (n < 0.1, n > 0.9)) r of
       [(True,_),(True,_),(_,True),(_,True)] -> do
          hClose csv
          putStrLn "Done"
          print n
       _ -> uloop $ (backprop learningrate [0,0] [0] . backprop learningrate [0,1] [1] . backprop learningrate [1,1] [0] . backprop learningrate [1,0] [1]) n
  uloop un
