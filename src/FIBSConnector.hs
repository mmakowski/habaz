module FIBSConnector ( FIBSConnector
                     , fibsConnector
                     ) 
where

import FIBSClient
import Events

data FIBSConnector = FIBSConnector (Event -> IO FIBSConnector)

fibsConnector :: EventQueue -> FIBSConnector
fibsConnector q = error "TODO"
