
module JonnyH.Board.Square where

import           GHC.Show
import           Protolude

data Square = Square {
      _file :: Char
    , _rank :: Int
} deriving (Eq, Ord)

instance Show Square where
    show (Square f r) = f : [intToDigit r]

