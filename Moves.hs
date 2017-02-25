
module Moves where

import Data.Maybe

import Piece
import Direction
import Square
import Board

pawnMoves :: Square -> Board -> [Square]
pawnMoves sqr@(Square f r piece@(Just (Piece White Pawn))) board =
    valids $
        [   neighbor sqr board North
        ,   neighbor sqr board NorthWest 
        ,   neighbor sqr board NorthEast 
        ] ++ if r == 2 then [at f 4 board | _rank sqr == 2] else []
pawnMoves sqr@(Square f r piece@(Just (Piece Black Pawn))) board =
    valids $
        [   neighbor sqr board South 
        ,   neighbor sqr board SouthWest 
        ,   neighbor sqr board SouthEast
        ] ++ if r == 2 then [at f 5 board | _rank sqr == 7] else []

bishopMoves :: Square -> Board -> [Square]
bishopMoves sqr board =
        apply bishopMove diagDirections
    where
        apply f = foldr ((++) . f) []
        bishopMove = inDirection sqr board

knightMoves :: Square -> Board ->[Square]
knightMoves sqr board =
            apply knightMove knightDirections
    where
        apply f = foldr (\(x, y) acc -> maybeToList (f x y) ++ acc) []
        knightMove dir1 dir2 =
            case neighbor sqr board dir2 of
                Nothing  -> Nothing
                Just sqr -> relative 2 sqr board dir1
    
rookMoves :: Square -> Board -> [Square]
rookMoves sqr board = apply rookMove fourDirections
    where
        apply f = foldr ((++) . f) []
        rookMove = inDirection sqr board

queenMoves :: Square -> Board -> [Square]
queenMoves sqr board = 
    bishopMoves sqr board ++ rookMoves sqr board

kingMoves :: Square -> Board -> [Square]
kingMoves sqr board =
    apply kingMove eightDirections
    where
        apply f = foldr (\x acc -> maybeToList (f x) ++ acc) []
        kingMove = neighbor sqr board

moves :: Char -> Int -> Board -> [Square]
moves f r board = 
    case at f r board of
        Just sqr@(Square _ _ (Just (Piece _ Pawn)))   -> pawnMoves sqr board
        Just sqr@(Square _ _ (Just (Piece _ Bishop))) -> bishopMoves sqr board
        Just sqr@(Square _ _ (Just (Piece _ Knight))) -> knightMoves sqr board
        Just sqr@(Square _ _ (Just (Piece _ Rook)))   -> rookMoves sqr board
        Just sqr@(Square _ _ (Just (Piece _ Queen)))  -> queenMoves sqr board
        Just sqr@(Square _ _ (Just (Piece _ King)))   -> kingMoves sqr board
        Nothing -> []

move :: Square -> Square -> Board -> Board
move = swapContent
