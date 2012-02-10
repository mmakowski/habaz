{-|
This module defines types that are not part of the Backgammon game but that represented
data passed between different modules of the system.
-}
module DomainTypes ( PlayerInfo (..) )
where

data PlayerInfo = PlayerInfo { name :: String
                             , rating :: Float
                             , experience :: Int
                             , canBeInvited :: Bool
                             }
  deriving (Show, Eq)
