
module JonnyH.Board.Next where

import Protolude

import JonnyH.Color
import JonnyH.Board.Square
import JonnyH.Board.Board
import JonnyH.Moves

-------------------------------------------------------------------------------
-- all legal possible positions after one move of the given color in the given
-- board

moveToPosition :: Board -> (Square, Square) -> Board
moveToPosition brd mv = move mv brd

movesToPositions :: Board -> (Square, [Square]) -> [Board]
movesToPositions brd (sqr, sqrs) =
    map (\sqr' -> moveToPosition brd (sqr, sqr')) sqrs

allPositions :: Board -> Color -> [Board]
allPositions brd col =
    let
        moves = allMoves col brd
    in concatMap (movesToPositions brd) moves


