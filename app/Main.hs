
module Main where

-- import Protolude        hiding (evaluate)
-- import Board.Board
import Color
import Board.Next
import Protolude
import Board.Initial
import GameTree

main :: IO ()
main =
    print $ evaluated initialBoard 3

