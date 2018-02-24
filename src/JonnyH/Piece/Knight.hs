
module JonnyH.Piece.Knight where

import           Protolude

import           JonnyH.Board
import           JonnyH.Direction
import           JonnyH.Square

moves :: Square -> Board -> [Square]
moves sqr b = apply knightMove dirs
    where
        apply f = foldr (\(x, y) acc -> maybeToList (f x y) ++ acc) []
        knightMove dir1 dir2 =
            case neighbor sqr b dir2 of
                Nothing -> Nothing
                Just s  -> relative 2 s b dir1
        dirs = [ (North, East)
               , (North, West)
               , (East, North)
               , (East, South)
               , (South, East)
               , (South, West)
               , (West, North)
               , (West, South)
               ]
