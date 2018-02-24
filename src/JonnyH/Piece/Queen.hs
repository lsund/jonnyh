
module JonnyH.Piece.Queen where

import           Protolude

import           JonnyH.Board
import           JonnyH.Piece.Bishop as Bishop
import           JonnyH.Piece.Rook   as Rook
import           JonnyH.Square


moves :: Square -> Board -> [Square]
moves sqr b =
    Bishop.moves sqr b ++ Rook.moves sqr b


