
module Parser where

import Data.List.Split

import Util
import Piece
import Square

datadir = "/home/lsund/Data/chess-data/openings/pgn/"
file = "Hungarian.pgn"

sequences = do
    cont <- readFile $ datadir ++ file
    let moves = lines cont
        chunks = tail $ splitOn "\r1." $ concat $ filter (('[' /=) . head) moves
        sequences = map (splitOn " " . removeReturns) chunks
    return $ map (init . filter (not . null) . map trimNumber) sequences

starts n = do
    xs <- sequences
    return $ map (take n) xs


removeReturns = replace (== '\r') ' '

trimNumber (x : '.' : xs) = xs
trimNumber (x : y : '.' : xs) = xs
trimNumber xs = xs

