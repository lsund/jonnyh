
module Parser where

import              Protolude        hiding  (replace)
import              Data.List.Split

import              Util
import              Piece
import              Board.Square
import qualified    Data.List        as L
import qualified    Data.Text        as T

datadir = "/home/lsund/Data/chess-data/openings/pgn/"
file = "Hungarian.pgn"

sequences = do
    cont <- readFile $ datadir ++ file
    let moves = L.lines (T.unpack cont)
        chunks = L.tail $ splitOn "\r1." $ concat $ filter (('[' /=) . L.head) moves
        sequences = map (splitOn " " . removeReturns) chunks
    return $ map (L.init . filter (not . null) . map trimNumber) sequences

starts n = do
    xs <- sequences
    return $ map (take n) xs


removeReturns = replace (== '\r') ' '

trimNumber (x : '.' : xs) = xs
trimNumber (x : y : '.' : xs) = xs
trimNumber xs = xs

