
module Board where

import Data.List.Split

import Square
import Piece

newtype Board = Board { _squares :: [Square] } deriving (Show)

initialBoard :: Board
initialBoard = Board $ map initialSquare [(x, y) | x <- files, y <- ranks]

initialSquare :: (Char, Int) -> Square
initialSquare ('a', 1) = Square 'a' 1 (Just ( Piece White Rook))
initialSquare ('b', 1) = Square 'b' 1 (Just ( Piece White Knight))
initialSquare ('c', 1) = Square 'c' 1 (Just ( Piece White Bishop))
initialSquare ('d', 1) = Square 'd' 1 (Just ( Piece White Queen))
initialSquare ('e', 1) = Square 'e' 1 (Just ( Piece White King))
initialSquare ('f', 1) = Square 'f' 1 (Just ( Piece White Bishop))
initialSquare ('g', 1) = Square 'g' 1 (Just ( Piece White Knight))
initialSquare ('h', 1) = Square 'h' 1 (Just ( Piece White Rook))
initialSquare ( f , 2) = Square f   2 (Just ( Piece White Pawn))
initialSquare ( f , 7) = Square f   7 (Just ( Piece Black Pawn))
initialSquare ('a', 8) = Square 'a' 8 (Just ( Piece Black Rook))
initialSquare ('b', 8) = Square 'b' 8 (Just ( Piece Black Knight))
initialSquare ('c', 8) = Square 'c' 8 (Just ( Piece Black Bishop))
initialSquare ('d', 8) = Square 'd' 8 (Just ( Piece Black Queen))
initialSquare ('e', 8) = Square 'e' 8 (Just ( Piece Black King))
initialSquare ('f', 8) = Square 'f' 8 (Just ( Piece Black Bishop))
initialSquare ('g', 8) = Square 'g' 8 (Just ( Piece Black Knight))
initialSquare ('h', 8) = Square 'h' 8 (Just ( Piece Black Rook))
initialSquare (f, r)   = Square f   r Nothing

ranks :: [Int]
ranks = [1..8]

files :: String
files = ['a'..'h']

valids :: [Square] -> [Square]
valids = filter valid

valid :: Square -> Bool
valid (Square x y _) = elem x files && elem y ranks

empty :: Square -> Bool
empty (Square _ _ Nothing) = True
empty _                    = False

squareInDirection :: Direction -> Square -> Int -> Square
squareInDirection dir (Square f r c) n = 
    case dir of 
        North     -> Square f nNorth c
        NorthEast -> Square nEast nNorth c
        East      -> Square nEast r c
        SouthEast -> Square nEast nSouth c
        South     -> Square f nSouth c
        SouthWest -> Square nWest nSouth c
        West      -> Square nWest r c
        NorthWest -> Square nWest nNorth c
    where 
        nNorth    = iterate succ r !! n
        nEast     = iterate succ f !! n
        nSouth    = iterate pred r !! n
        nWest     = iterate pred f !! n

freeInDirection :: Square -> Direction -> [Square]
freeInDirection sqr dir = 
    let reverseSeq = foldl (\xs n -> squareInDirection dir sqr n : xs) [] [1..8]
    in reverse $ takeWhile free reverseSeq
    where
        free sqr = empty sqr && valid sqr

squaresInDirection :: Square -> Direction -> [Square]
squaresInDirection sqr dir = 
    let frees = freeInDirection sqr dir
    in frees ++ nextIfValid frees dir
    where
        nextIfValid seq dir 
            | valid (last seq) = [squareInDirection dir sqr 1]
            | otherwise      = []

