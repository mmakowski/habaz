module View.MatchWindow ( createMatchWindow )
where

-- WX
import Graphics.UI.WX 

import Backgammon (initialBoard)
import View.Board

startWidth, startHeight :: Int
startWidth = 800
startHeight = 800

createMatchWindow :: Frame () -> IO (Frame ())
createMatchWindow f = do 
  mw <- frame [ text := "Match" 
              -- TODO: , parent := f 
              ]
  p <- panel mw [on paint := paintBoard initialBoard]
  set mw [ layout := minsize (sz startWidth startHeight) $ fill $ widget p ]

  return mw
