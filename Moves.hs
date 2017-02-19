
module Moves (moves) where

import Piece
import Square
import Board

pawnMoves :: Square -> Board -> [Square]
pawnMoves sqr@(Square file 2 piece@(Just (Piece White Pawn))) board = 
    valids  (
            [   squareInDirection North sqr 1 board
            ,   squareInDirection NorthWest sqr 1  board
            ,   squareInDirection NorthEast sqr 1 board
            ] ++ [Just (Square file 4 piece) | _rank sqr == 2]
            )
pawnMoves sqr@(Square file 7 piece@(Just (Piece Black Pawn))) board = 
    valids  (
            [   squareInDirection South sqr 1 board
            ,   squareInDirection SouthWest sqr 1 board
            ,   squareInDirection SouthEast sqr 1 board
            ] ++ [Just (Square file 5 piece) | _rank sqr == 7]
            )
pawnMoves _ _ = undefined

bishopMoves :: Square -> Board -> [Square]
bishopMoves sqr board =
        bishopMove NorthEast
    ++  bishopMove SouthEast
    ++  bishopMove SouthWest
    ++  bishopMove NorthWest
    where
        bishopMove dir = squaresInDirection sqr dir board

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
            case squareInDirection dir2 sqr 1 board of
                Nothing  -> Nothing
                Just sqr -> squareInDirection dir1 sqr 2 board
    
apply :: (Direction -> [Square]) -> [Direction] -> [Square]
apply f = foldr ((++) . f) []

rookMoves :: Square -> Board -> [Square]
rookMoves sqr board =
        rookMove North
    ++  rookMove East
    ++  rookMove South
    ++  rookMove West
    where
        f = apply rookMove [North, East, South, West]
        rookMove dir = squaresInDirection sqr dir board 

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
        kingMove dir = squareInDirection dir sqr 1 board

moves :: Piece -> Square -> Board -> [Square]
moves piece@(Piece _ Pawn)   = pawnMoves
moves piece@(Piece _ Bishop) = bishopMoves
moves piece@(Piece _ Knight) = knightMoves
moves piece@(Piece _ Rook)   = rookMoves
moves piece@(Piece _ Queen)  = queenMoves
moves piece@(Piece _ King)   = kingMoves

