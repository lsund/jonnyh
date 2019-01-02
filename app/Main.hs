
module Main where

import           Protolude

import           JonnyH.Initial
import           JonnyH.Update
import           JonnyH.Square
import           JonnyH.Color
-- import           JonnyH.MinMax
-- import           JonnyH.Positions
import JonnyH.Database.Update as Database.Update
import JonnyH.Database.Query as Database.Query
import           PGNParser.PGNParser

import           PGNParser.Data.Move

dummyMoves = [Move 1 "d4" "d5"]

main :: IO ()
main = do
    xs <- Database.Query.response dummyMoves Black (2 :: Int)
    print xs

    -- let b' = update initialBoard (Square 'd' 2, Square 'd' 4)
    -- print b'
    -- let (Evaluation a b) = minmax' 4 (tree' White [b2])
    -- print a
    -- print b

