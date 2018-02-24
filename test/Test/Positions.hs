
module Test.Positions where

import           Data.Map
import           Protolude

import           JonnyH.Board
import           JonnyH.Color
import           JonnyH.Piece.Common
import           JonnyH.Square


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
