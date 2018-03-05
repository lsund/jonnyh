
module JonnyH.MoveParser where

import           Protolude

import           JonnyH.Board
import           JonnyH.Color
import           JonnyH.Piece.Pawn
import           JonnyH.Square

parse :: Color -> Board -> Text -> Maybe Square
parse c b "d4" = source c (Square 'd' 4) b
parse _ _ _    = undefined
