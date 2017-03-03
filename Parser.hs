
module Parser where

import Data.List.Split

import Util
import Piece
import Square

datadir = "/home/lsund/Data/chess-data/openings/pgn/"
file = "Hungarian.pgn"

read = do
    cont <- readFile $ datadir ++ file
    let moves = take 40 $ lines cont
        chunks = tail $ splitOn "\r1." $ concat $ filter (('[' /=) . head) moves
        tokens = filter (not . null) $ splitOn " " $ removeReturns $ head chunks
    return $ init $ map trimNumber tokens

removeReturns = replace (== '\r') ' '

trimNumber (x : '.' : xs) = xs
trimNumber (x : y : '.' : xs) = xs
trimNumber xs = xs

-- noteToMove :: String -> (Pice, Square)
-- noteToMove [f, r] = Pawn
