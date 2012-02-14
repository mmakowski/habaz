module View.PlayerList ( createPlayerList
                       , removePlayer
                       , selectedPlayer
                       , updatePlayer                       
                       ) 
where
-- WX
import Graphics.UI.WX 
import Graphics.UI.WXCore

import DomainTypes

createPlayerList :: Window a -> IO (ListCtrl ())
createPlayerList p = listCtrl p [ columns := [ ("Name", AlignLeft, 120)
                                             , ("Rating", AlignLeft, 70)
                                             , ("Experience", AlignLeft, 70)
                                             , ("Invitable", AlignLeft, 50)
                                             ]
                                ]

selectedPlayer :: ListCtrl () -> IO (Maybe String)
selectedPlayer = selectedPlayer' 0

selectedPlayer' :: Int -> ListCtrl () -> IO (Maybe String)
selectedPlayer' pos ctl = do
  itemCount <- listCtrlGetItemCount ctl
  if pos >= itemCount then return Nothing
    else do
      state <- listCtrlGetItemState ctl pos wxLIST_STATE_SELECTED
      if state == wxLIST_STATE_SELECTED then (return . Just) =<< listCtrlGetItemText ctl pos
        else selectedPlayer' (pos + 1) ctl

removePlayer :: ListCtrl () -> String -> IO ()
removePlayer listCtrl name = do
  (pos, currentName, itemCount) <- playerPosAttrs listCtrl name
  if pos < itemCount && currentName == name then listCtrlDeleteItem listCtrl pos else return False
  return ()

updatePlayer :: ListCtrl () -> PlayerInfo -> IO ()
updatePlayer listCtrl pinfo@(PlayerInfo name _ _ _) = do
  (pos, currentName, itemCount) <- playerPosAttrs listCtrl name
  let action = if itemCount <= pos || currentName /= name then insertItem else updateItem 
  action listCtrl pos pinfo
  
playerPosAttrs :: ListCtrl () -> String -> IO (Int, String, Int)
playerPosAttrs ctl name = do
  pos <- findPlayerPos ctl name
  itemCount <- listCtrlGetItemCount ctl
  currentName <- listCtrlGetItemText ctl pos
  return (pos, currentName, itemCount)

findPlayerPos :: ListCtrl () -> String -> IO Int
findPlayerPos ctl name = do
  itemCount <- listCtrlGetItemCount ctl
  findPlayerPos' ctl 0 itemCount name

findPlayerPos' :: ListCtrl () -> Int -> Int -> String -> IO Int
findPlayerPos' ctl lower upper name
  | lower == upper = return lower
  | otherwise      = do
    let pos = (lower + upper) `div` 2
    text <- listCtrlGetItemText ctl pos
    case name `compare` text of 
      EQ -> return pos
      LT -> findPlayerPos' ctl lower pos name
      GT -> if pos == upper - 1 then return upper else findPlayerPos' ctl pos upper name

type ItemAction = ListCtrl () -> Int -> PlayerInfo -> IO ()

insertItem :: ItemAction
insertItem ctl pos (PlayerInfo name rating exp inv) = do
  listCtrlInsertItemWithData ctl pos name
  updateAttrs ctl pos rating exp inv

updateItem :: ItemAction
updateItem ctl pos (PlayerInfo _ rating exp inv) = updateAttrs ctl pos rating exp inv

updateAttrs :: ListCtrl () -> Int -> Float -> Int -> Bool -> IO ()
updateAttrs ctl pos rating exp inv = do
  listCtrlSetItem ctl pos 1 (show rating) 0
  listCtrlSetItem ctl pos 2 (show exp) 0
  listCtrlSetItem ctl pos 3 (showB inv) 0
  return ()

showB :: Bool -> String
showB True = "Y"
showB False = ""