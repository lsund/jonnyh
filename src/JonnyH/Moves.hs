
module JonnyH.Moves where

import qualified Data.Map          as Map
import           Protolude

import           JonnyH.Board
import           JonnyH.Color
import           JonnyH.Piece.Common
import           JonnyH.Piece.King as King
import           JonnyH.Square

movesFrom :: Board -> Square -> [Square]
movesFrom b sqr =
    case Map.lookup sqr $ _position b of
        Just (Piece col pce) ->
            notOccupiedBy col b $ case pce of
                Pawn   -> notOccupiedBy (succ col) b $ pawnMoves sqr col b
                Bishop -> bishopMoves sqr b
                Knight -> knightMoves sqr b
                Rook   -> rookMoves sqr b
                Queen  -> queenMoves sqr b
                King   -> King.moves sqr b
        Nothing             -> []


allMoves :: Color -> Board -> [(Square, Square)]
allMoves col b = concatMap (\(y, ys) -> zip (repeat y) ys) allMovesFrom
    where
        filteredMap = Map.filter (ofColor col) $ _position b
        filteredSquares = Map.keys filteredMap
        ofColor col'' (Piece col' _) = col'' == col'
        allMovesFrom = zip filteredSquares $ map (movesFrom b) filteredSquares

