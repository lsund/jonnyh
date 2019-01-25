
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
import System.Random

import           PGNParser.Data.Move

-- dummyMoves = [ (1 (Square 'd' 4, Square 'd' 5)
--              , (2, (Square 'c' 4, Square 'e', 6)
--             , Move 3 "Nc3" "Nf6"]

main :: IO ()
main = do
    print "blah"
    -- xs <- Database.Query.response dummyMoves Black
    -- print xs

    -- let b' = update initialBoard (Square 'd' 2, Square 'd' 4)
    -- print b'
    -- let (Evaluation a b) = minmax' 4 (tree' White [b2])
    -- print a
    -- print b

