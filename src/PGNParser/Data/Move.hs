
module PGNParser.Data.Move where

import Prelude (String)
import Protolude

data Move = Move Int String String
          | GameResult String String
          | Unfinished
          deriving (Eq, Show)

isMove :: Move -> Bool
isMove Move{} = True
isMove _      = False
