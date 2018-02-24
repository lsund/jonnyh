
module JonnyH.Piece.Rook where

import Protolude

import JonnyH.Board
import JonnyH.Square
import JonnyH.Direction

moves :: Square -> Board -> [Square]
moves sqr b = apply rookMove [North, East, South, West]
    where
        apply f = foldr ((++) . f) []
        rookMove = untilOccupied sqr b


