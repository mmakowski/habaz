{-# LANGUAGE RecordWildCards #-}
{-|
Functions to create and operate on the list control that displays invitations.
-}
module View.InvitationList ( createInvitationList
                           , addInvitation
                           )
where

-- WX
import Graphics.UI.WX 
import Graphics.UI.WXCore

import Backgammon
import DomainTypes

createInvitationList :: Window a -> IO (ListCtrl ())
createInvitationList p = listCtrl p [ columns := [ ("Name", AlignLeft, 120)
                                                 , ("Rating", AlignLeft, 70)
                                                 , ("Experience", AlignLeft, 70)
                                                 , ("Match Length", AlignLeft, 100)
                                                 ]
                                    ]

nameCol = 0
ratingCol = 1
experienceCol = 2
matchLengthCol = 3

addInvitation :: ListCtrl () -> PlayerInfo -> MatchLength -> IO ()
addInvitation ctl (PlayerInfo {..}) len = do
  lastPos <- listCtrlGetItemCount ctl
  listCtrlInsertItemWithData ctl lastPos name
  listCtrlSetItem ctl lastPos ratingCol      (show rating) 0
  listCtrlSetItem ctl lastPos experienceCol  (show experience) 0
  listCtrlSetItem ctl lastPos matchLengthCol (showMatchLength len) 0
  return ()

showMatchLength :: MatchLength -> String
showMatchLength (NoOfPoints n)       = show n ++ " points"
showMatchLength UnlimitedMatchLength = "unlimited"
