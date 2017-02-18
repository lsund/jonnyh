
module Moves (moves) where

import Piece
import Square
import Board

pawnMoves :: Square -> [Square]
pawnMoves sqr@(Square file 2 piece@(Just (Piece White Pawn))) = 
    valids  [   squareInDirection North sqr 1
            ,   squareInDirection NorthWest sqr 1 
            ,   squareInDirection NorthEast sqr 1
            ]
            ++ [Square file 4 piece | _rank sqr == 2]
pawnMoves sqr@(Square file 7 piece@(Just (Piece Black Pawn))) = 
    valids  [   squareInDirection South sqr 1
            ,   squareInDirection SouthWest sqr 1
            ,   squareInDirection SouthEast sqr 1
            ]
            ++ [Square file 5 piece | _rank sqr == 7]
pawnMoves _ = undefined

bishopMoves :: Square -> [Square]
bishopMoves sqr =
        squaresInDirection sqr NorthEast
    ++  squaresInDirection sqr SouthEast
    ++  squaresInDirection sqr SouthWest
    ++  squaresInDirection sqr NorthWest

knightMoves :: Square -> [Square]
knightMoves sqr =
    valids  [   squareInDirection North   (squareInDirection East   sqr 1) 2
            ,   squareInDirection North   (squareInDirection West   sqr 1) 2
            ,   squareInDirection East    (squareInDirection North  sqr 1) 2
            ,   squareInDirection East    (squareInDirection South  sqr 1) 2
            ,   squareInDirection South   (squareInDirection East   sqr 1) 2
            ,   squareInDirection South   (squareInDirection West   sqr 1) 2
            ,   squareInDirection West    (squareInDirection North  sqr 1) 2
            ,   squareInDirection West    (squareInDirection South  sqr 1) 2
            ]

rookMoves :: Square -> [Square]
rookMoves sqr =
        squaresInDirection sqr North
    ++  squaresInDirection sqr East
    ++  squaresInDirection sqr South
    ++  squaresInDirection sqr West

queenMoves :: Square -> [Square]
queenMoves sqr = bishopMoves sqr ++ rookMoves sqr

kingMoves :: Square -> [Square]
kingMoves sqr =
    valids  [   squareInDirection North       sqr 1
            ,   squareInDirection NorthEast   sqr 1
            ,   squareInDirection East        sqr 1
            ,   squareInDirection SouthEast   sqr 1
            ,   squareInDirection South       sqr 1
            ,   squareInDirection SouthWest   sqr 1
            ,   squareInDirection West        sqr 1
            ,   squareInDirection NorthWest   sqr 1
            ]

moves :: Piece -> Square -> [Square]
moves piece@(Piece _ Pawn)      sqr = pawnMoves sqr
moves piece@(Piece _ Bishop)    sqr = bishopMoves sqr
moves piece@(Piece _ Knight)    sqr = knightMoves sqr
moves piece@(Piece _ Rook)      sqr = rookMoves sqr
moves piece@(Piece _ Queen)     sqr = queenMoves sqr
moves piece@(Piece _ King)      sqr = kingMoves sqr

