
module Initial where

import qualified Data.Map as Map

import Board
import Square
import Piece

board :: Map Square Piece -> Board
board = Board [Square f r | f <- ['a'..'h'], r <- [1..8]]

initialBoard :: Board
initialBoard = 
    Board [Square f r | f <- ['a'..'h'], r <- [1..8]] initialPosition

initialPosition :: Map Square Piece
initialPosition = Map.fromList $
    [
      (Square 'a' 1, Piece White Rook)
    , (Square 'b' 1, Piece White Knight)
    , (Square 'c' 1, Piece White Bishop)
    , (Square 'd' 1, Piece White Queen)
    , (Square 'e' 1, Piece White King)
    , (Square 'f' 1, Piece White Bishop)
    , (Square 'g' 1, Piece White Knight)
    , (Square 'h' 1, Piece White Rook)
    , (Square 'a' 8, Piece Black Rook)
    , (Square 'b' 8, Piece Black Knight)
    , (Square 'c' 8, Piece Black Bishop)
    , (Square 'd' 8, Piece Black Queen)
    , (Square 'e' 8, Piece Black King)
    , (Square 'f' 8, Piece Black Bishop)
    , (Square 'g' 8, Piece Black Knight)
    , (Square 'h' 8, Piece Black Rook)
    ]
    ++ [(Square f 2, Piece White Pawn) | f <- ['a'..'h']]
    ++ [(Square f 7, Piece Black Pawn) | f <- ['a'..'h']]

-------------------------------------------------------------------------------
-- tests

testQueen = board $ Map.fromList
    [
      (Square 'd' 8, Piece White Pawn)
    , (Square 'd' 4, Piece White Queen)
    , (Square 'b' 4, Piece Black Queen)
    ]

testKnight = board $ Map.fromList
    [
      (Square 'e' 5, Piece White Pawn)
    , (Square 'g' 4, Piece White Knight)
    , (Square 'e' 3, Piece Black Queen)
    ]

testPawn = board $ Map.fromList
    [
      (Square 'g' 4, Piece Black Pawn)
    , (Square 'f' 3, Piece Black Queen)
    ]
    
testKing = board $ Map.fromList
    [
      (Square 'd' 4, Piece Black King)
    , (Square 'd' 3, Piece Black Pawn)
    , (Square 'd' 5, Piece White Pawn)
    ]
