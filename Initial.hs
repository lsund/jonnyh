
module Initial where

import Board
import Square
import Piece

initialBoard :: Board
initialBoard = Board $ map initialSquare [(x, y) | x <- ['a'..'h'], y <- [1..8]]

initialSquare :: (Char, Int) -> Square
initialSquare ('a', 1) = Square 'a' 1 (Just (Piece White Rook))
initialSquare ('b', 1) = Square 'b' 1 (Just (Piece White Knight))
initialSquare ('c', 1) = Square 'c' 1 (Just (Piece White Bishop))
initialSquare ('d', 1) = Square 'd' 1 (Just (Piece White Queen))
initialSquare ('e', 1) = Square 'e' 1 (Just (Piece White King))
initialSquare ('f', 1) = Square 'f' 1 (Just (Piece White Bishop))
initialSquare ('g', 1) = Square 'g' 1 (Just (Piece White Knight))
initialSquare ('h', 1) = Square 'h' 1 (Just (Piece White Rook))
initialSquare ( f , 2) = Square f   2 (Just (Piece White Pawn))
initialSquare ( f , 7) = Square f   7 (Just (Piece Black Pawn))
initialSquare ('a', 8) = Square 'a' 8 (Just (Piece Black Rook))
initialSquare ('b', 8) = Square 'b' 8 (Just (Piece Black Knight))
initialSquare ('c', 8) = Square 'c' 8 (Just (Piece Black Bishop))
initialSquare ('d', 8) = Square 'd' 8 (Just (Piece Black Queen))
initialSquare ('e', 8) = Square 'e' 8 (Just (Piece Black King))
initialSquare ('f', 8) = Square 'f' 8 (Just (Piece Black Bishop))
initialSquare ('g', 8) = Square 'g' 8 (Just (Piece Black Knight))
initialSquare ('h', 8) = Square 'h' 8 (Just (Piece Black Rook))
initialSquare (f, r)   = Square f   r Nothing

