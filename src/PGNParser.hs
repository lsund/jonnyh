
module PGNParser where

import Prelude                                  (String)
import Protolude                        hiding  (try, (<|>), many, option)
import Text.ParserCombinators.Parsec
import Text.Parsec.Prim                 hiding  (try)

symbol :: String -> Parser (String, Maybe Int)
symbol s = parsecMap (\x -> (x, Nothing)) $ string s <* spaces


type PGNParser u = ParsecT String u Identity

data Movetext    = Move String String String | Result String String | Unfinished

data Tag         = Tag String String

data Game        = Game [Tag] [Movetext]


metaLabels :: [String]
metaLabels = [ "Event"
             , "Site"
             , "Date"
             , "Round"
             , "WhiteElo"
             , "BlackElo"
             , "White"
             , "Black"
             , "Result"
             , "ECO"]


parseSingleMove :: PGNParser u String
parseSingleMove = many1 (oneOf "abcdefgh12345678NBRQKxOO+-=")


parseMove :: PGNParser u Movetext
parseMove = do
    n <- many1 digit
    _ <- char '.'
    w <- parseSingleMove
    _ <- space
    b <- option "" parseSingleMove
    return $ Move n w b

parseResult :: PGNParser u Movetext
parseResult = do
    w <- try (string "1/2") <|> string "1" <|> string "0"
    _ <- char '-'
    b <- try (string "1/2") <|> string "1" <|> string "0"
    return $ Result w b

parseUnfinished :: PGNParser u Movetext
parseUnfinished = do
    _ <- char '*'
    return Unfinished

parseMoveLine :: PGNParser u [Movetext]
parseMoveLine = sepBy (try parseMove <|> try parseResult <|> parseUnfinished) (many $ char ' ')

parseMetaLine :: PGNParser () Tag
parseMetaLine = do
    _ <- symbol "["
    e <- choice $ map (try . string) metaLabels
    spaces
    _ <- symbol "\""
    s <- many $ noneOf "\""
    _ <- string "\"]\n"
    return $ Tag e s

parseGame :: PGNParser () Game
parseGame = do
    meta  <- many1 parseMetaLine
    _     <- newline
    moves <- endBy parseMoveLine newline
    return $ Game meta (concat moves)

parseGames :: PGNParser () [Game]
parseGames = many parseGame

parseFile :: String -> IO (Either ParseError [Game])
parseFile = parseFromFile parseGames
