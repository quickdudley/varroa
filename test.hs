import Data.List
import System.Random
import Neural

main = do
  g <- getStdGen
  let un = randomNNet g [2,4,1]
  uloop un
  where
   uloop n = do
     let r = concatMap (feedforward n) [[0,0],[1,1],[1,0],[0,1]]
     print r
     case map (\n -> (n < 0.1, n > 0.9)) r of
       [(True,_),(True,_),(_,True),(_,True)] -> do
          putStrLn "Done"
          print n
       _ -> uloop $ (backprop 0.03 [0,0] [0] . backprop 0.03 [0,1] [1] . backprop 0.03 [1,1] [0] . backprop 0.03 [1,0] [1]) n

