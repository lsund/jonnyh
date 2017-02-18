
module Main where

import Pawn (pawnMoves)
import Bishop (bishopMoves)
import Chess
import Board
import Square

board :: Board
board = Board [Square x y | x <- columns, y <- rows]

moves :: Piece -> Square -> [Square]
moves piece@(Piece _ Pawn) sqr   = pawnMoves piece sqr
moves piece@(Piece _ Bishop) sqr = bishopMoves sqr

main = return ()
