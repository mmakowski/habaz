{-|
A model of Backgammon game.
-}
module Backgammon 
  ( 
  -- * Types
    Board
  , Peg
  , Player (..)
  , MatchLength (..)
  -- ** Field accessors
  , pegs
  , count
  , owner
  , (!)
  -- * Constants
  , initialBoard
  ) where
import Data.Array
import qualified Data.Map as Map

data Player = White | Black
            deriving (Eq, Ord, Show)

data GameState 
     = Roll { diceOwner :: Player, cubeOwner :: Maybe Player }
     | Move { diceOwner :: Player, cubeOwner :: Maybe Player }
     | Double { diceOwner :: Player }
     | Finished { winner :: Player }
     deriving (Eq, Show)

data Peg = Peg { owner :: Maybe Player, count :: Int }
         deriving (Eq, Show)

data Board 
     = Board { pegs :: Array Int Peg, bar :: Map.Map Player Int, home :: Map.Map Player Int }
     deriving (Eq, Show)
              
data MatchLength
     = NoOfPoints Int
     | UnlimitedMatchLength
     deriving (Eq, Show)

empty :: Peg
empty = Peg Nothing 0

white :: Int -> Peg
white = Peg (Just White)

black :: Int -> Peg
black = Peg (Just Black)

emptyBar :: Map.Map Player Int
emptyBar = Map.fromList [(White, 0), (Black, 0)]

emptyHome :: Map.Map Player Int
emptyHome = emptyBar

emptyBoard :: Board
emptyBoard = Board (array (1, 24) (zip [1..24] (repeat empty))) emptyBar emptyHome

initialBoard :: Board
initialBoard = Board (array (1, 24) (zip [1..24] [black 2, empty, empty, empty, empty, white 5, 
                                                  empty, white 3, empty, empty, empty, black 5,
                                                  white 5, empty, empty, empty, black 3, empty,
                                                  black 5, empty, empty, empty, empty, white 2]))
                      emptyBar emptyHome
