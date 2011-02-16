module Main where
import FIBSClient
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

barWidthRatio = 0.08
homeWidthRatio = 0.08

paintBoard :: DC a -> Rect -> IO ()
paintBoard dc viewArea = 
  do let quarterHeight = rectHeight viewArea `div` 2
         barWidth = round $ (fromIntegral (rectWidth viewArea)) * barWidthRatio
         homeWidth = round $ (fromIntegral (rectWidth viewArea)) * homeWidthRatio
         quarterWidth = (rectWidth viewArea - barWidth - homeWidth) `div` 2
     mapM_ (\p -> drawQuarter dc p (if pointY p > 0 then -1 else 1) quarterWidth quarterHeight) 
           [Point x y | x <- [0, quarterWidth + barWidth], y <- [0, quarterHeight * 2]]
                     
drawQuarter :: DC a -> Point -> Int -> Int -> Int -> IO ()
drawQuarter dc origin orientation width height =
  do let pegWidth = width `div` 6
         pegHeight = round (fromIntegral height * 0.8)
     drawPegs dc 6 origin orientation pegWidth pegHeight
  where
    drawPegs _ 0 _ _ _ _ = return ()
    drawPegs dc pegNum origin orientation width height =
      do let colour = if (pegNum + ((orientation + 1) `div` 2)) `mod` 2 == 1 then red else white
         set dc [brushColor := colour, brushKind := BrushSolid]
         drawPeg dc (pointAdd origin (Point (width * (6 - pegNum)) 0)) orientation width height
         drawPegs dc (pegNum - 1) origin orientation width height

drawPeg :: DC a -> Point -> Int -> Int -> Int -> IO ()
drawPeg dc origin orientation width height =
  polygon dc [origin, 
              (pointAdd origin (Point width 0)), 
              (pointAdd origin (Point (width `div` 2) (height*orientation)))] []