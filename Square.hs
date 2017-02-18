
module Square where

import Data.Char

data Square = Square {
      column :: Char
    , row    :: Int
}

instance Show Square where
    show (Square c r) = c : [intToDigit r]

data Direction  = North
                | NorthEast
                | East
                | SouthEast
                | South
                | SouthWest
                | West
                | NorthWest

rows :: [Int]
rows = [1..8]

columns :: String
columns = ['a'..'h']

inDirection :: Direction -> Square -> Int -> Square
inDirection dir (Square col row) n = 
    case dir of 
        North     -> Square col nNorth
        NorthEast -> Square nEast nNorth
        East      -> Square nEast row
        SouthEast -> Square nEast nSouth
        South     -> Square col nSouth
        SouthWest -> Square nWest nSouth
        West      -> Square nWest row
        NorthWest -> Square nWest nNorth
    where 
        nNorth    = iterate succ row !! n
        nEast     = iterate succ col !! n
        nSouth    = iterate pred row !! n
        nWest     = iterate pred col !! n

valids :: [Square] -> [Square]
valids = filter valid

valid :: Square -> Bool
valid (Square x y) = elem x columns && elem y rows

sequenceInDirection :: Square -> Direction -> [Square]
sequenceInDirection sqr dir = 
    let diag = foldl (\xs n -> inDirection dir sqr n : xs) [] [1..8]
    in (takeWhile valid . reverse) diag 

