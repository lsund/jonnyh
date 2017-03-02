
module Moves where

import Data.Maybe
import qualified Data.Map as Map

import Piece
import Direction
import Square
import Board

pawnMoves :: Square -> Color -> Board -> [Square]
pawnMoves sqr@(Square f r) col brd
    | col == White =
        catMaybes $
            [   neighbor sqr brd North
            ,   neighborIfOccupied sqr brd NorthWest 
            ,   neighborIfOccupied sqr brd NorthEast 
            ]
            ++ if r == 2 then [Just $ Square f 4 | _rank sqr == 2] else []
    | col == Black =
        catMaybes $
            [   neighbor sqr brd South 
            ,   neighborIfOccupied sqr brd SouthWest 
            ,   neighborIfOccupied sqr brd SouthEast
            ]
            ++ if r == 7 then [Just $ Square f 5 | _rank sqr == 7] else []

bishopMoves :: Square -> Board -> [Square]
bishopMoves sqr brd =
        apply bishopMove diagDirections
    where
        apply f = foldr ((++) . f) []
        bishopMove = untilOccupied sqr brd

knightMoves :: Square -> Board ->[Square]
knightMoves sqr brd =
            apply knightMove knightDirections
    where
        apply f = foldr (\(x, y) acc -> maybeToList (f x y) ++ acc) []
        knightMove dir1 dir2 =
            case neighbor sqr brd dir2 of
                Nothing  -> Nothing
                Just sqr -> relative 2 sqr brd dir1
    
rookMoves :: Square -> Board -> [Square]
rookMoves sqr brd = apply rookMove fourDirections
    where
        apply f = foldr ((++) . f) []
        rookMove = untilOccupied sqr brd

queenMoves :: Square -> Board -> [Square]
queenMoves sqr brd = 
    bishopMoves sqr brd ++ rookMoves sqr brd

kingMoves :: Square -> Board -> [Square]
kingMoves sqr brd =
    apply kingMove eightDirections
    where
        apply f = foldr (\x acc -> maybeToList (f x) ++ acc) []
        kingMove = neighbor sqr brd

movesFrom :: Board -> Square -> [Square]
movesFrom brd sqr = 
    case Map.lookup sqr $ _position brd of
        Just (Piece col pce) -> 
            notOccupiedBy col brd $ case pce of 
                Pawn   -> pawnMoves sqr col brd
                Bishop -> bishopMoves sqr brd
                Knight -> knightMoves sqr brd
                Rook   -> rookMoves sqr brd
                Queen  -> queenMoves sqr brd
                King   -> kingMoves sqr brd
        Nothing             -> []

allMoves :: Color -> Board -> [(Square, [Square])]
allMoves col brd = zip filteredSquares $ map (movesFrom brd) filteredSquares
    where
        filteredMap = Map.filter (ofColor col) $ _position brd
        filteredSquares = Map.keys filteredMap
        ofColor col (Piece col' _) = col == col'

moveToPosition :: Board -> (Square, Square) -> Board
moveToPosition brd (sqr, sqr') = Board.move sqr brd sqr'

movesToPositions :: Board -> (Square, [Square]) -> [Board]
movesToPositions brd (sqr, sqrs) = 
    map (\sqr' -> moveToPosition brd (sqr, sqr')) sqrs

allPositions :: Color -> Board -> [Board]
allPositions col brd =
    let 
        moves = allMoves col brd
    in concatMap (movesToPositions brd) moves

