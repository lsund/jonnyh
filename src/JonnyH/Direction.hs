
module JonnyH.Direction where

import           Protolude

data Direction  = North
                | NorthEast
                | East
                | SouthEast
                | South
                | SouthWest
                | West
                | NorthWest
                    deriving (Show)

fourDirections =
    [North, East, South, West]

eightDirections =
    [North, NorthEast, East, SouthEast, South, SouthWest, West, NorthWest]

diagDirections =
    [NorthEast, SouthEast, SouthWest, NorthWest]

knightDirections =
    [ (North, East)
    , (North, West)
    , (East, North)
    , (East, South)
    , (South, East)
    , (South, West)
    , (West, North)
    , (West, South)
    ]

