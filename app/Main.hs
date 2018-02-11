
module Main where

-- import Protolude        hiding (evaluate)
-- import Board.Board
-- import Board.Initial
-- import Moves
import Color
import Protolude
import MinMax
import Positions


main :: IO ()
main = do
    -- let i = minmax 3 (tree White b2)
    let (Evaluation a b) = minmax' 4 (tree' White [b2])
    print a
    print b
    -- print $ allMoves Black p

