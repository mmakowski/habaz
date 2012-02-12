{-|
When the application is started the first window seen by the user i session window. It provides the user
with a way to issue session commands like login, toggle ready state etc., as well as a list of logged in 
players, and a chat box where the shouts appear. Using the lit of players the user can issue match 
invitations.

TODO: display invitation status

-}
module View ( viewConsumer ) 
where
-- WX
import Graphics.UI.WX hiding (Event, Menu, menu, menuBar, when)
import qualified Graphics.UI.WX as WX (Menu, menuBar)
-- control structures
import qualified Data.Traversable as DT (sequence)
import Control.Monad (when)
-- Model
import Model
import Backgammon
import Events
import DomainTypes 
-- other view modules
import View.PlayerList
-- player map
import Data.Map (Map)
import qualified Data.Map as Map

-- | All view elements that need to be acessed by Controller.
data View = View { sessionWindow :: Frame ()
                 , sessionMenu :: SessionMenu
                 , playerList :: ListCtrl ()
                 , invitationList :: ListCtrl ()
                 }

-- | Menu items which need to be accessed by Controller.
data SessionMenu = SessionMenu { logInItem :: MenuItem ()
                               , logOutItem :: MenuItem ()
                               , readyItem :: MenuItem ()
                               , exitItem :: MenuItem ()
                               , matchPane :: WX.Menu ()
                               }

viewConsumer :: EventQueueWriter -> IO EventConsumer
viewConsumer q = do
  v <- createView q
  viewConsumer' v q

startWidth, startHeight :: Int
startWidth = 200
startHeight = 300

createView :: EventQueueWriter -> IO View
createView q = do
  f <- frame [ text := "Habaź" ]
  p <- panel f []
  (menuBar, menuRepr) <- createMenuBar
  s <- splitterWindow p []
  playerList <- createPlayerList s
  -- TODO: move to separate module
  invitationList <- listCtrl s [ columns := [ ("Name", AlignLeft, 120)
                                            , ("Rating", AlignLeft, 70)
                                            , ("Experience", AlignLeft, 70)
                                            , ("Match Length", AlignLeft, 100)
                                            ]
                               ]
  set f [ WX.menuBar := menuBar
        , layout := minsize (sz startWidth startHeight) $ container p $
                    fill $ hsplit s 5 200 (widget playerList)
                                          (widget invitationList)
        ]
  -- required to enable processing of events while a modal dialog is displayed
  timer f [ interval := 100, on command := return () ]
  let view = View f menuRepr playerList invitationList
  setHandlers view q
  return view
 
createMenuBar :: IO ([WX.Menu ()], SessionMenu)
createMenuBar = do
  session <- menuPane        [ text := "&Session" ]
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

  match <- menuPane          [ text := "&Match" ]
  invite <- menuItem match   [ text := "&Invite"
                             , enabled := False 
                             ]
  return ([session, match],
          SessionMenu logIn logOut ready exit match)


viewConsumer' :: View -> EventQueueWriter -> IO EventConsumer
viewConsumer' v q = return $ EventConsumer $ \e -> do
  processEvent e q v
  DT.sequence $ Just $ viewConsumer' v q

processEvent :: Event -> EventQueueWriter -> View -> IO ()
processEvent e q = case e of
  Error msg           -> showErrorMessage msg
  Info msg            -> showInfoMessage msg
  LoginSuccesful _    -> loginSuccesful
  PlayerRemoved n     -> playerRemoved n
  PlayerUpdated pi    -> playerUpdated pi
  ReadyOn             -> readyToggled True
  ReadyOff            -> readyToggled False
  _                   -> const $ return ()

loginSuccesful :: View -> IO ()
loginSuccesful v = do
  set (logInItem  $ sessionMenu v) [ enabled := False ]
  set (logOutItem $ sessionMenu v) [ enabled := True  ]
  set (readyItem  $ sessionMenu v) [ enabled := True  ]

playerRemoved :: String -> View -> IO ()
playerRemoved name v = removePlayer (playerList v) name

playerUpdated :: PlayerInfo -> View -> IO ()
playerUpdated pinfo v = updatePlayer (playerList v) pinfo

readyToggled :: Bool -> View -> IO ()
readyToggled b v = set (readyItem $ sessionMenu v) [ checked := b ]

-- * handlers

setHandlers :: View -> EventQueueWriter -> IO ()
setHandlers v@(View w menu playerList invitationList) q = do
  setCommandHandler (logInItem menu) $ logIn q v
  setCommandHandler (readyItem menu) $ putEvent q ToggleReadyRequest
  setCommandHandler (exitItem menu)  $ close w
  wrapMouseHandler  playerList       $ playerListMouseHandler q v

type CommandHandler = IO ()

setCommandHandler :: Commanding a => a -> CommandHandler -> IO ()
setCommandHandler c h = set c [ on command := h ]

logIn :: EventQueueWriter -> View -> CommandHandler
logIn q v = whenOK (promptForUsernameAndPassword $ sessionWindow v) $ uncurry $ requestLogin q v

type MouseHandler = EventMouse -> IO ()
type MouseHandlerWrapper = MouseHandler -> MouseHandler

wrapMouseHandler :: Reactive a => a -> MouseHandlerWrapper -> IO ()
wrapMouseHandler ctl wrapper = do
  existingHandler <- get ctl $ on mouse
  set ctl [ on mouse := wrapper existingHandler ]

playerListMouseHandler :: EventQueueWriter -> View -> MouseHandlerWrapper
playerListMouseHandler q v _ (MouseLeftDClick _ _) = whenOK (selectedPlayer $ playerList v) $ invite q v
playerListMouseHandler _ _ d e = d e

-- ** login and registrations

requestLogin :: EventQueueWriter -> View -> String -> String -> IO ()
requestLogin q v user pass = do
  putEvent q $ AddEventConsumer "loginResultConsumer" $ loginResultConsumer q v user pass
  putEvent q $ LoginRequest user pass

loginResultConsumer :: EventQueueWriter -> View -> String -> String -> EventConsumer
loginResultConsumer q v user pass = EventConsumer $ \e -> case e of
  LoginSuccesful _ -> terminal $ return ()
  LoginFailed msg  -> terminal $ promptForRegistration msg q v user pass
  _                -> continue $ loginResultConsumer q v user pass
  where terminal op = do { op; terminate "loginResultConsumer" }

promptForRegistration :: String -> EventQueueWriter -> View -> String -> String -> IO ()
promptForRegistration msg q v user pass = do
  register <- promptYesNo v $ "There has been an error when logging in: \"" ++ msg ++ "\". " ++
                          "If you have not logged in for some time your account might have been removed. " ++
                          "Would you like to try to register a new account using the credentials you provided?"
  when register $ requestRegistration q v user pass

requestRegistration :: EventQueueWriter -> View -> String -> String -> IO ()
requestRegistration q v user pass = do
    putEvent q $ AddEventConsumer "registrationResultConsumer" $ registrationResultConsumer q v user pass
    putEvent q $ RegistrationRequest user pass
  
registrationResultConsumer :: EventQueueWriter -> View -> String -> String -> EventConsumer
registrationResultConsumer q v user pass = EventConsumer $ \e -> case e of
  RegistrationSuccesful  -> terminal $ logInAfterRegistration q v user pass
  RegistrationFailed msg -> terminal $ showErrorMessage ("Registration failed: \"" ++ msg ++ "\".") v
  _                      -> continue $ registrationResultConsumer q v user pass
  where terminal op = do { op; terminate "registrationResultConsumer" }

logInAfterRegistration :: EventQueueWriter -> View -> String -> String -> CommandHandler
logInAfterRegistration q v user pass = do
  login <- promptYesNo v "Registration succesful! Would you like to log in using your new account?"
  when login $ requestLogin q v user pass

promptForUsernameAndPassword :: Frame () -> IO (Maybe (String, String))
promptForUsernameAndPassword w = do 
  d <- dialog w [ text := "Log In" ]
  usernameInput <- textEntry d []
  passwordInput <- textEntry d []
  ok <- button d [ text := "&OK" ]
  cancel <- button d [ text := "&Cancel" ]
  set d [ layout := margin 10 $ column 5 [
             grid 5 5 [[label "user name", widget usernameInput],
                       [label "password", widget passwordInput]],
             row 5 [ widget ok, widget cancel]
             ]
        ]
  showModal d (setActions ok cancel usernameInput passwordInput)
  where
    setActions ok cancel usernameInput passwordInput stop = do
      set ok [ on command := do
                  username <- get usernameInput text
                  password <- get passwordInput text
                  stop (Just (username, password)) ]
      set cancel [ on command := stop Nothing ]

-- ** invitations

invite :: EventQueueWriter -> View -> String -> IO ()
invite q v name = whenOK (promptForMatchLength (sessionWindow v) name) $ requestInvitation q name

requestInvitation :: EventQueueWriter -> String -> String -> IO ()
requestInvitation q name len = putEvent q $ InviteRequest name len

promptForMatchLength :: Frame () -> String -> IO (Maybe String)
promptForMatchLength w name = do
  d <- dialog w [ text := "Log In" ]
  matchLengthInput <- textEntry d [ text := "3" ]
  ok <- button d [ text := "&Yes" ]
  cancel <- button d [ text := "&No" ]
  set d [ layout := margin 10 $ column 5 [ row 5 [ label $ "Invite " ++ name]
                                         , grid 5 5 [[ label $ "Invite " ++ name ++ " to"
                                                     , widget matchLengthInput
                                                     , label "point match?"
                                                     ]]
                                         , row 5 [ widget ok, widget cancel]
                                         ]
        ]
  showModal d (setActions ok cancel matchLengthInput)
  where
    setActions ok cancel matchLengthInput stop = do
      set ok     [ on command := get matchLengthInput text >>= (stop . Just) ]
      set cancel [ on command := stop Nothing ]

-- * generic dialogues

promptYesNo :: View -> String -> IO Bool
promptYesNo v prompt = confirmDialog (sessionWindow v) "Confirm" prompt True

showInfoMessage :: String -> View -> IO ()
showInfoMessage msg v = infoDialog (sessionWindow v) "Info" msg

showErrorMessage :: String -> View -> IO ()
showErrorMessage msg v = errorDialog (sessionWindow v) "Error" msg

-- * helpers

-- | a helper function to invoke action if user has pressed "OK" in dialog box
-- or, in general, when IO action returned non-empty result
whenOK :: IO (Maybe a) -> (a -> IO ()) -> IO ()
whenOK ioResult action = do
  result <- ioResult
  maybe (return()) action result

{- 

-- ** Board drawing

barWidthRatio = 0.08
homeWidthRatio = 0.08

paintBoard :: Board -> DC a -> Rect -> IO ()
paintBoard board dc viewArea = 
  do let quarterHeight = rectHeight viewArea `div` 2
         barWidth = round $ fromIntegral (rectWidth viewArea) * barWidthRatio
         homeWidth = round $ fromIntegral (rectWidth viewArea) * homeWidthRatio
         quarterWidth = (rectWidth viewArea - barWidth - homeWidth) `div` 2
         quarterOrigins = [Point (quarterWidth + barWidth) (quarterHeight * 2),
                           Point 0 (quarterHeight * 2),
                           Point 0 0,
                           Point (quarterWidth + barWidth) 0]
         pegSets = map (map (pegs board !)) [[1..6], [7..12], [13..18], [19..24]]
         drawQWPegsFromOrig (p, o) = drawQuarter p dc o (if pointY o > 0 then -1 else 1) 
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
                 pointAdd origin (Point width 0), 
                 pointAdd origin (Point (width `div` 2) (height * orientation))] 
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
-}