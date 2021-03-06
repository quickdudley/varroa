import Control.Monad
import Control.Monad.Identity
import Control.Monad.Writer
import Control.Concurrent
import Control.DeepSeq
import Data.IORef
import Control.Concurrent.MVar
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import System.Directory (doesFileExist,renameFile)
import System.IO
import System.IO.Unsafe
import System.Random
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

import Arithmetic
import Board
import Neural
import Reinforcement
import DrawBoard

data VLS = VLS {
  teacher :: NNet,
  student :: NNet,
  lead :: Int
 } deriving (Read,Show)

main = do
  board <- newIORef M.empty
  c <- doesFileExist "brain"
  vls <- if c
    then liftM read (readFile "brain")
    else liftM initialVLS getStdGen
  teacher vls `deepseq` student vls `deepseq` return ()
  unsafeInitGUIForThreadedRTS
  window <- windowNew
  onDestroy window mainQuit
  set window [
    windowTitle := "Varroa Neural network self-training",
    windowDefaultWidth := 480, windowDefaultHeight := 428
   ]
  frame <- frameNew
  containerAdd window frame
  canvas <- drawingAreaNew
  cEventBox <- eventBoxNew
  containerAdd frame cEventBox
  containerAdd cEventBox canvas
  widgetModifyBg canvas StateNormal (Color 65535 65535 65535)
  on canvas exposeEvent $ liftIO (do
    (w,h) <- widgetGetSize canvas
    drw <- widgetGetDrawWindow canvas
    b <- readIORef board
    renderWithDrawable drw
      (renderBoard (fromIntegral w) (fromIntegral h) Waiting b)
    return True
   )
  widgetShowAll window
  forkOS $ foldr1 (>=>) (repeat (train1game board (canvas, undefined))) vls
    >> return ()
  mainGUI

initialVLS g = let
  n = randomNNet g [1638,700,30,6]
  in VLS {
    teacher = n,
    student = n,
    lead = 0
   }

train1game boardRef gui@(canvas,_) vls' = do
  vls <- if lead vls' < 5
    then do
      putStrLn $ case lead vls' `compare` 0 of
        EQ -> "Current lead is even"
        GT -> "Student leads by " ++ show (lead vls')
        LT -> "Teacher leads by " ++ show (abs (lead vls'))
      return vls'
    else do
      putStrLn "The teacher becomes the student!"
      return $ vls' {teacher = student vls', lead = 0}
  let
    td = selfTrain (teacher vls) (student vls)
  rs <- liftM randomA newStdGen
  let
    gr = appEndo (runIdentity $ execWriterT $ runDecodeT td rs) []
    (p,b) = head gr
    sp = head $ M.keys $ M.filter isStudent p
  writeIORef boardRef b
  postGUIAsync $ widgetQueueDraw canvas
  putStrLn ((show $ M.size p) ++ " player game. " ++ show sp ++ " is student.")
  commentary boardRef gui vls gr

commentary _ _ vls [] = return vls
commentary boardRef gui@(canvas,_) vls [(p,b)] = do
  writeIORef boardRef b
  postGUIAsync $ widgetQueueDraw canvas
  let w = head $ whichPlayers b
  putStrLn $ "\n" ++ show w ++ " wins."
  let sp = head $ M.keys $ M.filter isStudent p
  return $ vls {lead = (if sp == w then (+) else (-)) (lead vls) 1}
commentary boardRef gui@(canvas,_) vls ((_,b'):r@((p,b):_)) = do
  let vls' = updateVLS p vls
  saveVLS vls' "brain"
  writeIORef boardRef b
  postGUIAsync $ widgetQueueDraw canvas
  putStr "."
  hFlush stdout
  let elim = whichPlayers b' \\ whichPlayers b
  sequence $ map (\e -> putStrLn $ "\n" ++ show e ++ " has lost.") elim
  vls' `seq` commentary boardRef gui vls' r

updateVLS p vls = maybe vls (\n -> vls {student = n})
  (actorNNet $ head $ filter isStudent $ M.elems p)

savelock :: MVar VLS
savelock = unsafePerformIO newEmptyMVar

saveVLS vls fn = forkIO $ do
  l <- tryPutMVar savelock vls
  if l
  then do
    let tfn = (fn ++ "~")
    h <- openFile tfn WriteMode
    hPutStr h $ show vls
    hClose h
    renameFile tfn fn
    takeMVar savelock >> return ()
  else return ()

