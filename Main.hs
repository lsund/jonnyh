
module Main where

import Piece
import Board
import Square

board :: Board
board = Board [Square x y | x <- columns, y <- rows]

main = return ()

