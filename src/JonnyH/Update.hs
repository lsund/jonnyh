
module JonnyH.Update where

import           Control.Arrow ((>>>))
import           Data.Map      (delete, insert)
import           Data.Maybe
import           Protolude

import           JonnyH.Board
import           JonnyH.Color
import           JonnyH.Moves


update :: Board -> Move -> Board
update brd (src, dst) = board $ (delete src >>> delete dst >>> insert dst pce) pos
    where
        pce = fromJust $ occupiedBy src brd
        pos = _position brd


allUpdates :: Board -> Color -> [Board]
allUpdates b col = map (update b) (allMoves col b)
