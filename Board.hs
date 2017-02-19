
module Board where

import Data.List.Split
import Data.List
import Data.Maybe

import Square
import Piece

newtype Board = Board { _squares :: [Square] } deriving (Show)

initialBoard :: Board
initialBoard = Board $ map initialSquare [(x, y) | x <- files, y <- ranks]

initialSquare :: (Char, Int) -> Square
initialSquare ('a', 1) = Square 'a' 1 (Just (Piece White Rook))
initialSquare ('b', 1) = Square 'b' 1 (Just (Piece White Knight))
initialSquare ('c', 1) = Square 'c' 1 (Just (Piece White Bishop))
initialSquare ('d', 1) = Square 'd' 1 (Just (Piece White Queen))
initialSquare ('e', 1) = Square 'e' 1 (Just (Piece White King))
initialSquare ('f', 1) = Square 'f' 1 (Just (Piece White Bishop))
initialSquare ('g', 1) = Square 'g' 1 (Just (Piece White Knight))
initialSquare ('h', 1) = Square 'h' 1 (Just (Piece White Rook))
initialSquare ( f , 2) = Square f   2 (Just (Piece White Pawn))
initialSquare ( f , 7) = Square f   7 (Just (Piece Black Pawn))
initialSquare ('a', 8) = Square 'a' 8 (Just (Piece Black Rook))
initialSquare ('b', 8) = Square 'b' 8 (Just (Piece Black Knight))
initialSquare ('c', 8) = Square 'c' 8 (Just (Piece Black Bishop))
initialSquare ('d', 8) = Square 'd' 8 (Just (Piece Black Queen))
initialSquare ('e', 8) = Square 'e' 8 (Just (Piece Black King))
initialSquare ('f', 8) = Square 'f' 8 (Just (Piece Black Bishop))
initialSquare ('g', 8) = Square 'g' 8 (Just (Piece Black Knight))
initialSquare ('h', 8) = Square 'h' 8 (Just (Piece Black Rook))
initialSquare (f, r)   = Square f   r Nothing

ranks :: [Int]
ranks = [1..8]

files :: String
files = ['a'..'h']

empty :: Square -> Bool
empty (Square _ _ Nothing) = True
empty _                    = False

findSquare :: Square -> Board -> Maybe Square
findSquare sqr (Board sqrs) = find (== sqr) sqrs

valids :: [Maybe Square] -> [Square]
valids msqrs = map fromJust $ filter isJust msqrs

squareInDirection :: Direction -> Square -> Int -> Board -> Maybe Square
squareInDirection dir (Square f r c) n board = 
    case dir of 
        North     -> findSquare (Square f       nNorth  c) board
        NorthEast -> findSquare (Square nEast   nNorth  c) board
        East      -> findSquare (Square nEast   r       c) board
        SouthEast -> findSquare (Square nEast   nSouth  c) board
        South     -> findSquare (Square f       nSouth  c) board
        SouthWest -> findSquare (Square nWest   nSouth  c) board
        West      -> findSquare (Square nWest   r       c) board
        NorthWest -> findSquare (Square nWest   nNorth  c) board
    where 
        nNorth    = iterate succ r !! n
        nEast     = iterate succ f !! n
        nSouth    = iterate pred r !! n
        nWest     = iterate pred f !! n

freeInDirection :: Square -> Direction -> Board -> [Square]
freeInDirection sqr dir board = 
    let reverseSeq = foldl nth [] [1..8]
    in reverse $ takeWhile empty (reverse reverseSeq)
    where
        nth xs n = 
            case squareInDirection dir sqr n board of
                Just sqr -> sqr : xs
                Nothing -> [] ++ xs

squaresInDirection :: Square -> Direction -> Board -> [Square]
squaresInDirection sqr dir board = 
    let frees = freeInDirection sqr dir board
    in frees ++ nextIfExist frees dir board
    where
        nextIfExist seq dir board
            | null seq               = []
            | otherwise              = [fromJust (squareInDirection dir sqr 1 board)]

