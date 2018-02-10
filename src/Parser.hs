
module Parser where

import              Prelude                  (String)
import              Protolude        hiding  (replace)
import              Data.List.Split

import              Util
import qualified    Data.List        as L
import qualified    Data.Text        as T


datadir :: FilePath
datadir = "/home/lsund/Data/chess-data/openings/pgn/"
file :: FilePath
file = "Hungarian.pgn"

sequences :: IO [[String]]
sequences = do
    cont <- readFile $ datadir ++ file
    let moves = L.lines (T.unpack cont)
        chunks = L.tail $ splitOn "\r1." $ concat $ filter (('[' /=) . L.head) moves
        ss = map (splitOn " " . removeReturns) chunks
    return $ map (L.init . filter (not . null) . map trimNumber) ss

starts :: Int -> IO [[String]]
starts n = do
    xs <- sequences
    return $ map (take n) xs


removeReturns :: String -> String
removeReturns = replace (== '\r') ' '

trimNumber :: String -> String
trimNumber (_ : '.' : xs) = xs
trimNumber (_ : _ : '.' : xs) = xs
trimNumber xs = xs

