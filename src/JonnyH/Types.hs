module JonnyH.Types where

import Protolude
import JonnyH.Square
import JonnyH.Piece.Common

type Position = Map Square Piece

type Movement = (Square, Square)

