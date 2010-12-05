module Main where
import Graphics.UI.WX

startWidth, startHeight :: Int
startWidth = 400
startHeight = 300

main :: IO ()
main = start habazWindow

habazWindow :: IO ()
habazWindow =
  do f <- frame [text := "Habaz"]
     p <- panel f [on paint := paintBoard]
     set f [layout := minsize (sz startWidth startHeight) $ widget p]
       
paintBoard :: DC a -> Rect -> IO ()
paintBoard dc viewArea = 
  do set dc [brushColor := red, brushKind := BrushSolid]
     polygon dc [(Point 0 0), (Point 20 0), (Point 10 100)] []

