module View.MatchWindow ( createMatchWindow )
where

-- WX
import Graphics.UI.WX 
import Graphics.UI.WXCore.WxcDefs

import Backgammon (initialBoard)
import View.Board

startWidth, startHeight :: Int
startWidth = 800
startHeight = 800

createMatchWindow :: Frame a -> IO (Frame ())
createMatchWindow f = do 
  fw <- get f frameParent
  mw <- frameEx wxDEFAULT_FRAME_STYLE [ text := "Match" ] fw
  p <- panel mw [on paint := paintBoard initialBoard]
  set mw [ layout := minsize (sz startWidth startHeight) $ fill $ widget p
         , on resize := do newSize <- get mw clientSize
                           set p [outerSize := newSize]
                           repaint p]
  return mw
