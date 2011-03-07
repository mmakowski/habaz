{-|
When the application is started the first window seen by the user i session window. It provides the user
with a way to issue session commands like login, toggle ready state etc., as well as a list of logged in 
players, and a chat box where the shouts appear. Using the lit of players the user can issue match 
invitations.

TODO: display invitation status

-}
module View(
  -- * Representation  
  View (..), Menu (..),
  -- ** Wrappers for UI toolkit
  setCommandHandler,
  -- * Actions
  ViewUpdate,
  (|>),
  disableLogIn, enableLogIn,
  disableLogOut, enableLogOut,
  disableReady, enableReady, setCheckedReady,
  closeMainWindow,
  showInfoMessage, showErrorMessages,
  -- * Construction
  createView
) where
-- WX
import Graphics.UI.WX hiding (Menu, menu, menuBar)
import qualified Graphics.UI.WX as WX (Menu, menuBar)
-- Model
import Model
import Backgammon -- TODO: re-export from Model
-- Misc functions
import Data.List (intercalate)

-- * Representation

-- | All view elements that need to be acessed by Controller.
data View = View { mainWindow :: Frame ()
                 , menu :: Menu
                 }

-- | Menu items which need to be accessed by Controller.
data Menu = Menu { logInItem :: MenuItem ()
                 , logOutItem :: MenuItem ()
                 , readyItem :: MenuItem ()
                 , exitItem :: MenuItem ()
                 }

-- ** Wrappers for UI toolkit

type CommandHandler = IO ()
setCommandHandler :: Commanding a => a -> CommandHandler -> IO ()
setCommandHandler c h = set c [ on command := h ]

-- * Actions

type ViewUpdate = View -> IO ()

-- | Composition of ViewUpdates
(|>) :: ViewUpdate -> ViewUpdate -> ViewUpdate
u1 |> u2 = \v -> do u1 v; u2 v

disableLogIn = setMenuEnabled logInItem False
enableLogIn = setMenuEnabled logInItem True
disableLogOut = setMenuEnabled logOutItem False
enableLogOut = setMenuEnabled logOutItem True
disableReady = setMenuEnabled readyItem False
enableReady = setMenuEnabled readyItem True
setCheckedReady = setMenuChecked readyItem
  
setMenuEnabled = setMenuBoolProp enabled
setMenuChecked = setMenuBoolProp checked
setMenuBoolProp prop itemAcc b v = set (itemAcc $ menu v) [ prop := b ]

closeMainWindow v = close $ mainWindow v

showInfoMessage :: String -> ViewUpdate
showInfoMessage msg v = infoDialog (mainWindow v) "Info" msg

showErrorMessages :: [String] -> ViewUpdate
showErrorMessages [] _ = return ()
showErrorMessages msgs v = errorDialog (mainWindow v) "Error" (intercalate "\n\n" msgs)

-- TODO: displaySession :: SessionState -> ViewUpdate

-- * Construction

startWidth, startHeight :: Int
startWidth = 500
startHeight = 500

createView :: IO View
createView =
  do f <- frame [ text := "Habaź" ]
     p <- panel f [ on paint := paintBoard initialBoard -- TODO
                  , on click := \p -> infoDialog f "dupa" (show p)
                  ]
     (menuBar, menuRepr) <- createMenuBar
     set f [ WX.menuBar := menuBar
           , layout := minsize (sz startWidth startHeight) $ widget p
           , on resize := do newSize <- get f clientSize
                             set p [outerSize := newSize]
                             repaint p
           ]
     timer f [ interval := 1, on command := return () ]
     return $ View f menuRepr

createMenuBar :: IO ([WX.Menu ()], Menu)
createMenuBar = do
  session <- menuPane        [ text := "&Session"]
  logIn <- menuItem session  [ text := "Log &In..." ]
  logOut <- menuItem session [ text := "Log &Out"
                             , enabled := False
                             ]
  ready <- menuItem session  [ text := "&Ready"
                             , checkable := True
                             , enabled := False
                             ]
  menuLine session
  exit <- menuItem session   [ text := "E&xit\tAlt+F4" ]
  return ([session],
          Menu logIn logOut ready exit)

-- ** Board drawing

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
