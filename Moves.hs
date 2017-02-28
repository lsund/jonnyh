
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
            ,   neighbor sqr brd NorthWest 
            ,   neighbor sqr brd NorthEast 
            ]
            ++ if r == 2 then [Just $ Square f 4 | _rank sqr == 2] else []
    | col == Black =
        catMaybes $
            [   neighbor sqr brd South 
            ,   neighbor sqr brd SouthWest 
            ,   neighbor sqr brd SouthEast
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

moves :: Square -> Board -> [Square]
moves sqr brd = 
    case Map.lookup sqr $ _pieces brd of
        Just (Piece col Pawn)   -> pawnMoves sqr col brd
        Just (Piece _ Bishop)   -> bishopMoves sqr brd
        Just (Piece col Knight) -> notOccupiedBy col brd $ knightMoves sqr brd
        Just (Piece _ Rook)     -> rookMoves sqr brd
        Just (Piece _ Queen)    -> queenMoves sqr brd
        Just (Piece _ King)     -> kingMoves sqr brd
        Nothing                 -> []

