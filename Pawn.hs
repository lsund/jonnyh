
module Pawn where

import Chess

pawnMoves :: Piece -> Square -> [Square]
pawnMoves (Piece White Pawn) (col, 2)   = 
    valids [(col, 4), (col, 3), (pred col, 3), (succ col, 3)]
pawnMoves (Piece White Pawn) (col, row) = 
    valids [(col, succ row), (pred col, succ row), (succ col, succ row)]
pawnMoves (Piece Black Pawn) (col, 7) = 
    valids [(col, 5), (col, 6), (pred col, 6), (succ col, 6)]
pawnMoves (Piece Black Pawn) (col, row) = 
    valids [(col, pred row), (pred col, pred row), (succ col, pred row)]
pawnMoves (Piece _ _) _ = undefined

valids = filter (\(x, y) -> elem x rows && elem y columns)

