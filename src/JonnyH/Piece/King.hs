
module JonnyH.Piece.King where

import Protolude

import           JonnyH.Square
import           JonnyH.Board

moves :: Square -> Board -> [Square]
moves sqr b =
    apply kingMove dirs
    where
        apply f = foldr (\x acc -> maybeToList (f x) ++ acc) []
        kingMove = neighbor sqr b
        dirs = [ North
               , NorthEast
               , East
               , SouthEast
               , South
               , SouthWest
               , West
               , NorthWest]


