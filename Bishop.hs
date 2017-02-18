
module Bishop where

import Chess
import Square

bishopMoves :: Square -> [Square]
bishopMoves sqr =
        sequenceInDirection sqr NorthEast
    ++  sequenceInDirection sqr SouthEast
    ++  sequenceInDirection sqr SouthWest
    ++  sequenceInDirection sqr NorthWest

