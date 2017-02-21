
module Moves where

import Piece
import Square
import Board

import Data.Maybe

pawnMoves :: Square -> Board -> [Square]
pawnMoves sqr@(Square f 2 piece@(Just (Piece White Pawn))) board = 
    valids  (
            [   relative sqr 1 board North
            ,   relative sqr 1  board NorthWest 
            ,   relative sqr 1 board NorthEast 
            ] ++ [at f 4 board | _rank sqr == 2]
            )
pawnMoves sqr@(Square f 7 piece@(Just (Piece Black Pawn))) board = 
    valids  (
            [   relative sqr 1 board South 
            ,   relative sqr 1 board SouthWest 
            ,   relative sqr 1 board SouthEast
            ] ++ [at f 5 board | _rank sqr == 7]
            )
pawnMoves _ _ = error "tbi"

bishopMoves :: Square -> Board -> [Square]
bishopMoves sqr board =
        bishopMove NorthEast
    ++  bishopMove SouthEast
    ++  bishopMove SouthWest
    ++  bishopMove NorthWest
    where
        bishopMove = inDirection sqr board

knightMoves :: Square -> Board ->[Square]
knightMoves sqr board =
    valids  [   knightMove North East
            ,   knightMove North West
            ,   knightMove East North
            ,   knightMove East South
            ,   knightMove South East
            ,   knightMove South West
            ,   knightMove West North
            ,   knightMove West South
            ]
    where 
        knightMove dir1 dir2 =
            case relative sqr 1 board dir2 of
                Nothing  -> Nothing
                Just sqr -> relative sqr 2 board dir1
    
apply :: (Direction -> [Square]) -> [Direction] -> [Square]
apply f = foldr ((++) . f) []

rookMoves :: Square -> Board -> [Square]
rookMoves sqr board = f
    where
        f = apply rookMove [North, East, South, West]
        rookMove = inDirection sqr board

queenMoves :: Square -> Board -> [Square]
queenMoves sqr board = 
    bishopMoves sqr board ++ rookMoves sqr board

kingMoves :: Square -> Board -> [Square]
kingMoves sqr board =
    valids  [   kingMove North
            ,   kingMove NorthEast
            ,   kingMove East
            ,   kingMove SouthEast
            ,   kingMove South
            ,   kingMove SouthWest
            ,   kingMove West
            ,   kingMove NorthWest
            ]
    where
        kingMove = relative sqr 1 board

moves :: Char -> Int -> Board -> [Square]
moves f r board = 
    case at f r board of
        Just (sqr@(Square _ _ (Just (Piece _ Pawn))))   -> pawnMoves sqr board
        Just (sqr@(Square _ _ (Just (Piece _ Bishop)))) -> bishopMoves sqr board
        Just (sqr@(Square _ _ (Just (Piece _ Knight)))) -> knightMoves sqr board
        Just (sqr@(Square _ _ (Just (Piece _ Rook))))   -> rookMoves sqr board
        Just (sqr@(Square _ _ (Just (Piece _ Queen))))  -> queenMoves sqr board
        Just (sqr@(Square _ _ (Just (Piece _ King))))   -> kingMoves sqr board
        Nothing -> []

