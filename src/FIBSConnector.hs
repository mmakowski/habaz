module FIBSConnector ( FIBSConnector
                     , fibsConnector
                     ) 
where

import FIBSClient
import Events

data FIBSConnector = FIBSConnector (Event -> IO FIBSConnector)

fibsConnector :: EventQueue -> FIBSConnector
fibsConnector q = FIBSConnector $ loginTransition q

loginTransition :: EventQueue -> Event -> IO FIBSConnector
loginTransition q e = case e of
  LoginRequest user pass -> loginConnector q user pass
  _                      -> return $ fibsConnector q

loginConnector :: EventQueue -> String -> String -> IO FIBSConnector
loginConnector q user pass = error "TODO"