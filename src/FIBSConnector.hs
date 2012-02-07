module FIBSConnector ( FIBSConnector
                     , fibsConnector
                     ) 
where

import FIBSClient
import Events

data FIBSConnector = FIBSConnector (Event -> IO FIBSConnector)

fibsConnector :: EventQueue -> FIBSConnector
fibsConnector q = FIBSConnector $ loginOrRegisterTransition q

loginOrRegisterTransition :: EventQueue -> Event -> IO FIBSConnector
loginOrRegisterTransition q e = case e of
  LoginRequest user pass        -> loginConnector q user pass
  RegistrationRequest user pass -> registrationConnector q user pass
  _                             -> return $ fibsConnector q

loginConnector :: EventQueue -> String -> String -> IO FIBSConnector
loginConnector q user pass = error "TODO"

registrationConnector :: EventQueue -> String -> String -> IO FIBSConnector
registrationConnector q user pass = error "TODO"