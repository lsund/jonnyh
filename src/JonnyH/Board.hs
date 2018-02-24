
module JonnyH.Board where

import qualified Data.List           as List
import           Data.Map            (Map, elems, lookup)
import           GHC.Show
import           Prelude             ((!!))
import           Protolude           hiding (Map, evaluate, show)


import           JonnyH.Color
import           JonnyH.Direction
import           JonnyH.Piece.Common
import           JonnyH.Square


type Position = Map Square Piece

type Move = (Square, Square)

data Board = Board {
      _squares  :: [Square]
    , _position :: Position
}

instance Eq Board where
    x == y = evaluate x == evaluate y

instance Ord Board where
    x `compare` y = evaluate x `compare` evaluate y


instance Show Board where
    show b =
        "   A  B  C  D  E  F  G  H\n" ++
        concat (zipWith (\x ln -> show x ++ " " ++ ln) ([8,7..1] :: [Int]) rsRep')
        where
            rs = reverse $ ranks b
            rsRep = concatMap (strRep b)
            rsRep' = map ((++ "\n") . rsRep) rs
            strRep b' sqr =
                case occupiedBy sqr b' of
                    Just pce -> "[" ++ show pce ++ "]"
                    Nothing  -> "[ ]"


ranks :: Board -> [[Square]]
ranks = ranks' . _squares
    where
        ranks' sqrs       = map (`ofRank` sqrs) [1..8]
        ofRank r          = foldr (\s acc -> consIf (_rank s == r) s acc) []
        consIf True e acc = e : acc
        consIf _ _ acc    = acc


evaluate :: Board -> Int
evaluate b = sumPces whites - sumPces blacks
    where
        (whites, blacks) = List.partition isWhite $ elems pos
        sumPces = foldl (\acc pce -> value pce + acc) 0 :: [Piece] -> Int
        pos = _position b

board :: Position -> Board
board = Board [Square f r | f <- ['a'..'h'], r <- [1..8]]


occupied :: Square -> Board -> Bool
occupied sqr = isJust . lookup sqr . _position

occupiedBy :: Square -> Board -> Maybe Piece
occupiedBy sqr = lookup sqr . _position

occupiedByColor :: Square -> Board -> Maybe Color
occupiedByColor sqr b = _color <$> occupiedBy sqr b

notOccupiedBy :: Color -> Board -> [Square] -> [Square]
notOccupiedBy col b = filter (\x -> Just col /= occupiedByColor x b)

-------------------------------------------------------------------------------
-- get a single square

-- TODO
relative :: Int -> Square ->  Board -> Direction -> Maybe Square
relative n (Square f r) _ dir =
    boardSquare $
    let (f', r') = cords dir in Square f' r'
    where
        nNorth    = iterate succ r !! n
        nEast     = iterate succ f !! n
        nSouth    = iterate pred r !! n
        nWest     = iterate pred f !! n
        boardSquare sqr@(Square f' r')
            | f' `elem` ['a'..'h'] && r' `elem` [1..8] = Just sqr
            | otherwise                              = Nothing
        cords dir' =
            case dir' of
            North     -> (f    , nNorth)
            NorthEast -> (nEast, nNorth)
            East      -> (nEast, r     )
            SouthEast -> (nEast, nSouth)
            South     -> (f    , nSouth)
            SouthWest -> (nWest, nSouth)
            West      -> (nWest, r     )
            NorthWest -> (nWest, nNorth)

neighbor :: Square -> Board -> Direction -> Maybe Square
neighbor = relative 1

neighborIfOccupied :: Square -> Board -> Direction -> Maybe Square
neighborIfOccupied sqr b dir =
    neighbor sqr b dir >>= returnIf (`occupied` b)
    where
        returnIf p v = if p v then return v else empty

neighborIfNotOccupied :: Square -> Board -> Direction -> Maybe Square
neighborIfNotOccupied sqr b dir =
    neighbor sqr b dir >>= returnIf (not . flip occupied b)
    where
        returnIf p v = if p v then return v else empty

-------------------------------------------------------------------------------
-- get a sequence of squares

takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore p = foldr (\x ys -> if p x then x : ys else [x]) []

untilOccupied :: Square -> Board -> Direction -> [Square]
untilOccupied sqr b dir =
    let reverseSeq = foldl nth [] [1..8]
    in reverse $ takeWhileOneMore (not . flip occupied b) (reverse reverseSeq)
    where
        nth xs n =
            case relative n sqr b dir of
                Just s  -> s : xs
                Nothing -> [] ++ xs

