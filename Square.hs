
module Square where

type Square = (Char, Int)

data Direction  = North
                | NorthEast
                | East
                | SouthEast
                | South
                | SouthWest
                | West
                | NorthWest

column :: Square -> Char
column = fst

row :: Square -> Int
row = snd

neighbor :: Direction -> Square -> Square
neighbor = inDirection 1

inDirection :: Int -> Direction -> Square -> Square
inDirection n dir (col, row) = 
    case dir of 
        North     -> (col, nNorth)
        NorthEast -> (nEast, nNorth)
        East      -> (nEast, row)
        SouthEast -> (nEast, nSouth)
        South     -> (col, nSouth)
        SouthWest -> (nWest, nSouth)
        West      -> (nWest, row)
        NorthWest -> (nWest, nNorth)
    where 
        nNorth    = iterate succ row !! n
        nEast     = iterate succ col !! n
        nSouth    = iterate pred row !! n
        nWest     = iterate pred col !! n

            

