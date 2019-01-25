
module JonnyH.Ply where

import JonnyH.Piece.Common
import JonnyH.Square

data Ply = Move Piece Square
         | LongCastle
         | ShortCastle

