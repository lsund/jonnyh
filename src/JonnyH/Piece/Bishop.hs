
module JonnyH.Piece.Bishop where

import Protolude

import JonnyH.Board
import JonnyH.Square
import JonnyH.Direction

moves :: Square -> Board -> [Square]
moves sqr b = apply bishopMove [NorthEast, SouthEast, SouthWest, NorthWest]
    where
        apply f = foldr ((++) . f) []
        bishopMove = untilOccupied sqr b
