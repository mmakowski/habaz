module Main where
import Backgammon
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Array
import FIBSClient hiding (login)
import Graphics.UI.WX
import Session
import Transitions

startWidth, startHeight :: Int
startWidth = 500
startHeight = 500

main :: IO ()
main = 
  do sessionTV <- newTMVarIO initialSessionState -- TODO: this should be a TMVar
     msgsTV <- newTVarIO [] -- TODO: this should be a TChan
     forkIO $ readerThread sessionTV msgsTV
     start $ habazWindow sessionTV msgsTV

readerThread :: TMVar SessionState -> TVar [ParseResult FIBSMessage] -> IO ()
readerThread sessionTMV msgsTV = forever $ do 
  s <- atomically $ readTMVar sessionTMV
  putStr "" -- TODO: without this IO action the app hangs...
  case s of
    LoggedIn conn _ -> do incoming <- readMessages conn
                          incomingTV <- newTVarIO incoming -- TODO: doesn't have to be TVar, regular Var would do
                          readLoop incomingTV msgsTV
    _               -> return ()
  where
    readLoop inpTV outpTV = forever $ do 
      (msg:msgs) <- readTVarIO inpTV
      putStrLn $ "read: " ++ (show msg) -- TODO: without this there is a stack overflow
      append outpTV msg
      atomically $ writeTVar inpTV msgs
    append outpTV msg = atomically $ do
      outp <- readTVar outpTV
      writeTVar outpTV $ outp ++ [msg]
    
habazWindow :: TMVar SessionState -> TVar [ParseResult FIBSMessage] -> IO ()
habazWindow stateTMV msgsTV =
  do f <- frame [ text := "Habaz" ]
     p <- panel f [ on paint := paintBoard initialBoard
                  , on click := \p -> infoDialog f "dupa" (show p)
                  ]
     t <- timer f [ interval := 1000
                  , on command := gameCycle msgsTV stateTMV f
                  ]
     menu <- createMenuBar stateTMV
     set f [ menuBar := menu
           , layout := minsize (sz startWidth startHeight) $ widget p
           , on resize := do newSize <- get f clientSize
                             set p [outerSize := newSize]
                             repaint p
           ]

gameCycle msgsTV stateTMV f = do 
  msgs <- readAndReset msgsTV
  state <- atomically $ takeTMVar stateTMV
  state' <- updateState state msgs f
  putStrLn $ "state after update: " ++ (show state')
  atomically $ putTMVar stateTMV state'
  -- TODO: state UI actions
  where
    readAndReset msgsTV = atomically $ do 
      msgs <- readTVar msgsTV
      writeTVar msgsTV []
      return msgs
         
updateState s [] _ = return s
updateState s (h:t) f = do
  --infoDialog f "msg" (show h)
  s' <- transition h s
  updateState s' t f

createMenuBar :: TMVar SessionState -> IO [Menu ()]
createMenuBar stateTMV = do
  session <- menuPane [text := "&Session"]
  mLogin <- menuItem session [ text := "Log &In..." 
                             , on command := doLogin
                             ]
  return [session]
  where
    doLogin = applyStateTransition (login defaultFIBSHost defaultFIBSPort "habaztest_a" "habaztest") stateTMV

applyStateTransition :: SessionStateTransition -> TMVar SessionState -> IO ()
applyStateTransition trans stateTMV = do
  state <- atomically $ takeTMVar stateTMV
  state' <- trans state
  atomically $ putTMVar stateTMV state'

barWidthRatio = 0.08
homeWidthRatio = 0.08

paintBoard :: Board -> DC a -> Rect -> IO ()
paintBoard board dc viewArea = 
  do let quarterHeight = rectHeight viewArea `div` 2
         barWidth = round $ (fromIntegral (rectWidth viewArea)) * barWidthRatio
         homeWidth = round $ (fromIntegral (rectWidth viewArea)) * homeWidthRatio
         quarterWidth = (rectWidth viewArea - barWidth - homeWidth) `div` 2
         quarterOrigins = [Point (quarterWidth + barWidth) (quarterHeight * 2),
                           Point 0 (quarterHeight * 2),
                           Point 0 0,
                           Point (quarterWidth + barWidth) 0]
         pegSets = map (map (pegs board !)) [[1..6], [7..12], [13..18], [19..24]]
         drawQWPegsFromOrig = \(p, o) -> drawQuarter p dc o (if pointY o > 0 then -1 else 1) 
                                                     quarterWidth quarterHeight
     mapM_ drawQWPegsFromOrig (pegSets `zip` quarterOrigins)
     
drawQuarter :: [Peg] -> DC a -> Point -> Int -> Int -> Int -> IO ()
drawQuarter pegs dc origin orientation width height =
  do let pegWidth = width `div` 6
         pegHeight = round (fromIntegral height * 0.8)
     drawPegs (if orientation == -1 then reverse pegs else pegs) dc 6 pegWidth pegHeight
  where
    drawPegs _ _ 0 _ _ = return ()
    drawPegs (peg:pegs) dc pegNum width height =
      do let pegColour = if (pegNum + ((orientation + 1) `div` 2)) `mod` 2 == 1 then red else white
             pegOrigin = pointAdd origin (Point (width * (6 - pegNum)) 0)
         drawPeg peg dc pegOrigin orientation width height pegColour
         drawPegs pegs dc (pegNum - 1) width height

drawPeg :: Peg -> DC a -> Point -> Int -> Int -> Int -> Color -> IO ()
drawPeg peg dc origin orientation width height colour =
  do polygon dc [origin, 
                 (pointAdd origin (Point width 0)), 
                 (pointAdd origin (Point (width `div` 2) (height * orientation)))] 
             [brushColor := colour]
     let pieceColour = case owner peg of
           Just White -> white
           _ -> black
     drawPieces (count peg) dc origin orientation width height pieceColour

drawPieces :: Int -> DC a -> Point -> Int -> Int -> Int -> Color -> IO()
drawPieces n dc origin orientation width height colour = drawPieces' n n
  where
    drawPieces' 0 _ = return ()
    drawPieces' n total =
      do let radius = width `div` 2
             centre = pointAdd origin (Point radius (((n - 1) * 2 + 1) * radius * orientation))
         circle dc centre radius [brushColor := colour]
         drawPieces' (n - 1) total
