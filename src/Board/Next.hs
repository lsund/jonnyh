
module Board.Next where

import Square
import Piece
import Board
import Moves

-------------------------------------------------------------------------------
-- all legal possible positions after one move of the given color in the given
-- board

moveToPosition :: Board -> (Square, Square) -> Board
moveToPosition brd (sqr, sqr') = Board.move sqr brd sqr'

movesToPositions :: Board -> (Square, [Square]) -> [Board]
movesToPositions brd (sqr, sqrs) =
    map (\sqr' -> moveToPosition brd (sqr, sqr')) sqrs

allPositions :: Board -> Color -> [Board]
allPositions brd col =
    let
        moves = allMoves col brd
    in concatMap (movesToPositions brd) moves


