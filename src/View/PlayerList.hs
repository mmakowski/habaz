module View.PlayerList (
  createPlayerList,
  applyPlayerDeltas
) where
-- WX
import Graphics.UI.WX 
import Graphics.UI.WXCore (listCtrlDeleteItem, listCtrlGetItemCount, listCtrlGetItemText, listCtrlInsertItemWithData)
-- Model
import Model

createPlayerList :: Frame () -> IO (ListCtrl ())
createPlayerList f = listCtrl f [ columns := [ ("Name", AlignLeft, 120)
                                             , ("Rating", AlignLeft, 50)
                                             ]
                                ]


applyPlayerDeltas :: ListCtrl () -> PlayerMap -> Int -> [PlayerDelta] -> IO ()
applyPlayerDeltas _ _ _ [] = return ()
applyPlayerDeltas listCtrl playerMap pos (pd:pds) = do
  validPos <- isValidPos listCtrl pos (pdstr pd)
  if not $ validPos then applyPlayerDeltas listCtrl playerMap (pos + 1) (pd:pds)
    else do
      newPos <- applyPlayerDelta listCtrl playerMap pos pd
      applyPlayerDeltas listCtrl playerMap newPos pds

isValidPos :: ListCtrl () -> Int -> String -> IO Bool
isValidPos listCtrl pos pName = do
  itemCount <- listCtrlGetItemCount listCtrl
  let isAtEnd = pos == itemCount
  currentText <- listCtrlGetItemText listCtrl pos
  let currentItemMatches = currentText == pName
  let currentIsLarger = currentText > pName
  prevText <- listCtrlGetItemText listCtrl (pos - 1)
  let prevIsSmaller = pos == 0 || prevText < pName
  return $ isAtEnd || currentItemMatches || currentIsLarger && prevIsSmaller

applyPlayerDelta :: ListCtrl () -> PlayerMap -> Int -> PlayerDelta -> IO Int
applyPlayerDelta listCtrl playerMap pos (Updated pName) = do
  itemCount <- listCtrlGetItemCount listCtrl
  currentName <- listCtrlGetItemText listCtrl pos
  let action = if itemCount <= pos || currentName /= (pnstr pName) then insertItem else updateItem 
  action listCtrl playerMap pos pName
applyPlayerDelta listCtrl playerMap pos (Removed pName) = do
  itemCount <- listCtrlGetItemCount listCtrl
  currentName <- listCtrlGetItemText listCtrl pos
  if pos < itemCount && currentName == (pnstr pName) then listCtrlDeleteItem listCtrl pos else return False
  return pos  
-- TODO: added
  
type ItemAction = ListCtrl () -> PlayerMap -> Int -> PlayerName -> IO Int 

insertItem :: ItemAction
insertItem listCtrl playerMap pos pName = do
  listCtrlInsertItemWithData listCtrl pos (pnstr pName)
  return $ pos + 1

updateItem :: ItemAction
updateItem _ _ pos _ = return $ pos + 1 -- TODO
