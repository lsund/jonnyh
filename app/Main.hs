
module Main where

-- import Protolude        hiding (evaluate)
-- import Board.Board
import Color
import Protolude
import Board.Initial
import MinMax
import Positions
import Moves


main :: IO ()
main = do
    -- let i = minmax 3 (tree White b2)
    let (i, p) = minmax'' 4 (tree' White [b2])
    print i
    print p
    -- print $ allMoves Black p

