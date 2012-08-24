module View.InvitationList ( createInvitationList
                      )
where

-- WX
import Graphics.UI.WX 
import Graphics.UI.WXCore


createInvitationList :: Window a -> IO (ListCtrl ())
createInvitationList p = listCtrl p [ columns := [ ("Name", AlignLeft, 120)
                                                 , ("Rating", AlignLeft, 70)
                                                 , ("Experience", AlignLeft, 70)
                                                 , ("Match Length", AlignLeft, 100)
                                                 ]
                                    ]
