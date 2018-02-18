
module Main where

import           Protolude

import           JonnyH.Board.Initial
import           JonnyH.Board.Update
import           JonnyH.Board.Square
-- import           JonnyH.Color
-- import           JonnyH.MinMax
-- import           JonnyH.Positions

import           PGNParser.Data.MoveText

dummy = [ Move 1 "d4" "d5"
        ]

main :: IO ()
main = do
    let b' = update initialBoard (Square 'd' 2, Square 'd' 4)
    print b'
    -- let (Evaluation a b) = minmax' 4 (tree' White [b2])
    -- print a
    -- print b

