
module JonnyH.Direction where

import           Protolude

import           JonnyH.Color

data Direction  = North
                | NorthEast
                | East
                | SouthEast
                | South
                | SouthWest
                | West
                | NorthWest
                deriving (Show)

backwards :: Color -> Direction
backwards White = South
backwards Black = North
