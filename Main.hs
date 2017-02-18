
module Main where

import Piece
import Square
import Board
import Moves

board :: Board
board = Board [Square x y Nothing | x <- files, y <- ranks]

main = return ()

