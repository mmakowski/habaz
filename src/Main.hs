module Main where
import Model (initialSessionState)
import View (createView)
import Controller (controller)
import Graphics.UI.WX (start)

main :: IO ()
main = start $ do
  view <- createView
  controller initialSessionState view
  
