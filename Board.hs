
module Board where

import Data.List.Split

import Square

newtype Board = Board { _squares :: [Square] } deriving (Show)

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

