
module PGNParser.PGNParser where

import Prelude                                  (String, read)
import Protolude                        hiding  (try, (<|>), many, option)
import Text.ParserCombinators.Parsec
import Text.Parsec.Prim                 hiding  (try)

import PGNParser.Data.Metadata
import PGNParser.Data.Move

import qualified JonnyH.Ply as Ply (Ply(..))
import JonnyH.Square
import JonnyH.Piece.Common
import qualified JonnyH.Color as Color

type PGNParser u = ParsecT String u Identity

data Game = Game Metadata [Move]

parsePly :: PGNParser u Ply.Ply
parsePly = do
    piece <- oneOf "NBRQK"
    capture <- char 'x'
    file <- oneOf "abcdefgh"
    rank <- oneOf "12345678"
    return $ Ply.Move (Piece Color.White Knight) (Square file 1)

parseSingleMove =
    many1 (oneOf "abcdefgh12345678NBRQKxOO+-=")


parseMove :: PGNParser u Move
parseMove = do
    n <- many1 digit
    _ <- char '.'
    w <- parseSingleMove
    _ <- space
    b <- option "" parseSingleMove
    -- return $ ((read n :: Int), w b)
    return $ Move (read n :: Int) w b


parseResult :: PGNParser u Move
parseResult = do
    w <- try (string "1/2") <|> string "1" <|> string "0"
    _ <- char '-'
    b <- try (string "1/2") <|> string "1" <|> string "0"
    return $ GameResult w b


parseUnfinished :: PGNParser u Move
parseUnfinished = do
    _ <- char '*'
    return Unfinished


parseMoveLine :: PGNParser u [Move]
parseMoveLine = sepBy
                    (try parseMove <|> try parseResult <|> parseUnfinished)
                    (many $ char ' ')


parseMetaLine :: PGNParser () Tag
parseMetaLine = do
    _ <- char '['
    e <- choice $ map (try . string) metaLabels
    spaces
    _ <- char '"'
    s <- many $ noneOf "\""
    _ <- string "\"]\n"
    return $ Tag (stringToTagKey e) s


parseGame :: PGNParser () Game
parseGame = do
    meta  <- many1 parseMetaLine
    _     <- newline
    moves <- endBy parseMoveLine newline
    return $ Game (tl2meta meta) (concat moves)


parseGames :: PGNParser () [Game]
parseGames = many parseGame


parseFile :: String -> IO (Either ParseError [Game])
parseFile = parseFromFile parseGames
