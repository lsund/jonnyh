
module Main where

import Pawn (pawnMoves)
import Chess

board :: Board
board = Board [(x, y) | x <- rows, y <- columns]

moves :: Piece -> Square -> [Square]
moves piece@(Piece clr Pawn) sqr   = pawnMoves piece sqr
moves piece@(Piece clr Bishop) sqr = bishopMoves piece sqr

main = return ()
