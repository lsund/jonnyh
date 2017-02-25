
module Square where

import Data.Char

import Piece

data Square = Square {
      _file    :: Char
    , _rank    :: Int
    , _content :: Maybe Piece
}

instance Show Square where
    show (Square _ _ Nothing)                     = "[ ]"
    show (Square _ _ (Just (Piece Black Pawn)))   = "[p]"
    show (Square _ _ (Just (Piece Black Bishop))) = "[b]"
    show (Square _ _ (Just (Piece Black Knight))) = "[n]"
    show (Square _ _ (Just (Piece Black Rook)))   = "[r]"
    show (Square _ _ (Just (Piece Black Queen)))  = "[q]"
    show (Square _ _ (Just (Piece Black King)))   = "[k]"
    show (Square _ _ (Just (Piece White Pawn)))   = "[P]"
    show (Square _ _ (Just (Piece White Bishop))) = "[B]"
    show (Square _ _ (Just (Piece White Knight))) = "[N]"
    show (Square _ _ (Just (Piece White Rook)))   = "[R]"
    show (Square _ _ (Just (Piece White Queen)))  = "[Q]"
    show (Square _ _ (Just (Piece White King)))   = "[K]"
    -- show (Square f r _)   = f : show r

instance Eq Square where
    (Square f r _) == (Square f' r' _) = f == f' && r == r'

