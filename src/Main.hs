module Main where
import Model (initialSessionState)
import View (createView)
import Controller (controller)
import Graphics.UI.WX (start)

main :: IO ()
main = start $ do
  view <- createView
  controller initialSessionState view
  
{-
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
-}    
