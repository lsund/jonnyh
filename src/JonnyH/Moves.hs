
module JonnyH.Moves where

import qualified Data.Map            as Map
import           Protolude

import           JonnyH.Board
import           JonnyH.Color
import           JonnyH.Piece.Bishop as Bishop
import           JonnyH.Piece.Common
import           JonnyH.Piece.King   as King
import           JonnyH.Piece.Knight as Knight
import           JonnyH.Piece.Pawn   as Pawn
import           JonnyH.Piece.Queen  as Queen
import           JonnyH.Piece.Rook   as Rook
import           JonnyH.Square

reachableFrom :: Board -> Square -> [Square]
reachableFrom b sqr =
    case Map.lookup sqr $ _position b of
        Just (Piece col pce) ->
            notOccupiedBy col b $ case pce of
                Pawn   -> notOccupiedBy (succ col) b $ Pawn.moves sqr col b
                Bishop -> Bishop.moves sqr b
                Knight -> Knight.moves sqr b
                Rook   -> Rook.moves sqr b
                Queen  -> Queen.moves sqr b
                King   -> King.moves sqr b
        Nothing             -> []


allMoves :: Color -> Board -> [Movement]
allMoves col b = concatMap (\(y, ys) -> zip (repeat y) ys) allMovesFrom
    where
        filteredMap = Map.filter (ofColor col) $ _position b
        filteredSquares = Map.keys filteredMap
        ofColor col'' (Piece col' _) = col'' == col'
        allMovesFrom = zip filteredSquares $ map (reachableFrom b) filteredSquares

