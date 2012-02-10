module View.PlayerList ( createPlayerList
                       , removePlayer
                       , updatePlayer
                       ) 
where
-- WX
import Graphics.UI.WX 
import Graphics.UI.WXCore (listCtrlDeleteItem, listCtrlGetItemCount, listCtrlGetItemText, listCtrlInsertItemWithData, listCtrlSetItem)

import DomainTypes

{-
-- Player map
import qualified Data.Map as Map
-- Model
import Model
-- misc
import Data.Maybe (fromMaybe)
-}
createPlayerList :: Frame () -> IO (ListCtrl ())
createPlayerList f = listCtrl f [ columns := [ ("Name", AlignLeft, 120)
                                             , ("Rating", AlignLeft, 70)
                                             , ("Experience", AlignLeft, 70)
                                             , ("Invitable", AlignLeft, 50)
                                             ]
                                ]

removePlayer :: ListCtrl () -> String -> IO ()
removePlayer listCtrl name = do
  pos <- findPlayerPos listCtrl name
  itemCount <- listCtrlGetItemCount listCtrl
  currentName <- listCtrlGetItemText listCtrl pos
  if pos < itemCount && currentName == name then listCtrlDeleteItem listCtrl pos else return False
  return ()

updatePlayer :: ListCtrl () -> PlayerInfo -> IO ()
updatePlayer listCtrl pinfo@(PlayerInfo name _ _ _) = do
  pos <- findPlayerPos listCtrl name
  itemCount <- listCtrlGetItemCount listCtrl
  currentName <- listCtrlGetItemText listCtrl pos
  let action = if itemCount <= pos || currentName /= name then insertItem else updateItem 
  action listCtrl pos pinfo
  
findPlayerPos :: ListCtrl () -> String -> IO Int
findPlayerPos = findPlayerPos' 0

findPlayerPos' :: Int -> ListCtrl () -> String -> IO Int
findPlayerPos' pos listCtrl name = do
  validPos <- isValidPos listCtrl pos name
  if not validPos then findPlayerPos' (pos + 1) listCtrl name else return pos

isValidPos :: ListCtrl () -> Int -> String -> IO Bool
isValidPos listCtrl pos name = do
  itemCount <- listCtrlGetItemCount listCtrl
  let isAtEnd = pos == itemCount
  currentText <- listCtrlGetItemText listCtrl pos
  let currentItemMatches = currentText == name
  let currentIsLarger = currentText > name
  prevText <- listCtrlGetItemText listCtrl (pos - 1)
  let prevIsSmaller = pos == 0 || prevText < name
  return $ isAtEnd || currentItemMatches || currentIsLarger && prevIsSmaller

type ItemAction = ListCtrl () -> Int -> PlayerInfo -> IO ()

insertItem :: ItemAction
insertItem listCtrl pos (PlayerInfo name rating exp inv) = do
  listCtrlInsertItemWithData listCtrl pos name
  listCtrlSetItem listCtrl pos 1 (show rating) 0
  listCtrlSetItem listCtrl pos 2 (show exp) 0
  listCtrlSetItem listCtrl pos 3 (showB inv) 0
  return ()

updateItem :: ItemAction
updateItem listCtrl pos (PlayerInfo _ rating exp inv) = do
  listCtrlSetItem listCtrl pos 1 (show rating) 0
  listCtrlSetItem listCtrl pos 2 (show exp) 0
  listCtrlSetItem listCtrl pos 3 (showB inv) 0
  return ()

showB :: Bool -> String
showB True = "Y"
showB False = ""