
module Moves where

import Piece
import Direction
import Square
import Board

import Data.Maybe
import Data.List.Split

pawnMoves :: Square -> Board -> [Square]
pawnMoves sqr@(Square f r piece@(Just (Piece White Pawn))) board =
    valids $
        [   relative sqr 1 board North
        ,   relative sqr 1 board NorthWest 
        ,   relative sqr 1 board NorthEast 
        ] ++ if r == 2 then [at f 4 board | _rank sqr == 2] else []
pawnMoves sqr@(Square f r piece@(Just (Piece Black Pawn))) board =
    valids $
        [   relative sqr 1 board South 
        ,   relative sqr 1 board SouthWest 
        ,   relative sqr 1 board SouthEast
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
            case relative sqr 1 board dir2 of
                Nothing  -> Nothing
                Just sqr -> relative sqr 2 board dir1
    
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
        kingMove = relative sqr 1 board

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

-- move :: Square -> Board -> Board
-- move sqr@(Square f r (Just (Piece _ Pawn))) board =
--     let (x : y : []) = splitWhen (== sqr) (_squares board)
--     in  ( 

