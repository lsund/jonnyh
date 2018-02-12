
module JonnyH.Moves where

import Protolude
import qualified Data.Map as Map

import JonnyH.Color
import JonnyH.Board.Direction
import JonnyH.Board.Square
import JonnyH.Board.Board
import JonnyH.Piece

-------------------------------------------------------------------------------
-- piece moves

pawnMoves :: Square -> Color -> Board -> [Square]
pawnMoves sqr@(Square f r) White brd =
        catMaybes $
            [   neighbor sqr brd North
            ,   neighborIfOccupied sqr brd NorthWest
            ,   neighborIfOccupied sqr brd NorthEast
            ]
            ++ if r == 2 then [Just $ Square f 4 | _rank sqr == 2] else []
pawnMoves sqr@(Square f r) Black brd =
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

knightMoves :: Square -> Board -> [Square]
knightMoves sqr brd =
            apply knightMove knightDirections
    where
        apply f = foldr (\(x, y) acc -> maybeToList (f x y) ++ acc) []
        knightMove dir1 dir2 =
            case neighbor sqr brd dir2 of
                Nothing  -> Nothing
                Just s -> relative 2 s brd dir1

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

-------------------------------------------------------------------------------
-- Moves from a square

movesFrom :: Board -> Square -> [Square]
movesFrom brd sqr =
    case Map.lookup sqr $ _position brd of
        Just (Piece col pce) ->
            notOccupiedBy col brd $ case pce of
                Pawn   -> notOccupiedBy (succ col) brd $ pawnMoves sqr col brd
                Bishop -> bishopMoves sqr brd
                Knight -> knightMoves sqr brd
                Rook   -> rookMoves sqr brd
                Queen  -> queenMoves sqr brd
                King   -> kingMoves sqr brd
        Nothing             -> []

-------------------------------------------------------------------------------
-- all legal moves after one move of the given color in the given board

allMoves :: Color -> Board -> [(Square, [Square])]
allMoves col brd = zip filteredSquares $ map (movesFrom brd) filteredSquares
    where
        filteredMap = Map.filter (ofColor col) $ _position brd
        filteredSquares = Map.keys filteredMap
        ofColor col'' (Piece col' _) = col'' == col'

