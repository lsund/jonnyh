
module JonnyH.MoveParser where

import           Protolude

import           JonnyH.Board
import           JonnyH.Square

parse :: Board -> Text -> Move
parse b "d4" = (undefined, Square 'd' 4)
parse _ _    = undefined
