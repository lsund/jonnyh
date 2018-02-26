
module Test.Positions where

import           Data.Map
import           Protolude

import           JonnyH.Board
import           JonnyH.Color
import           JonnyH.Piece.Common
import           JonnyH.Square

-- Alone knight in the middle of board
p1 :: Board
p1 = board $ fromList
    [
      (Square 'c' 5, Piece Black Knight)
    ]

m1 = [ Square 'd' 7
     , Square 'b' 7
     , Square 'e' 6
     , Square 'e' 4
     , Square 'd' 3
     , Square 'b' 3
     , Square 'a' 6
     , Square 'a' 4
     ]


p2 :: Board
p2 = board $ fromList
    [
      (Square 'c' 5, Piece Black Bishop)
    , (Square 'f' 2, Piece White Pawn)
    ]

m2 = [ Square 'd' 4
     , Square 'e' 3
     , Square 'f' 2
     , Square 'b' 6
     , Square 'a' 7
     , Square 'b' 4
     , Square 'a' 3
     , Square 'd' 6
     , Square 'e' 7
     , Square 'f' 8
     ]

