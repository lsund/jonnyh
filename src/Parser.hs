
module Parser where

import Prelude                                  (String)
import Protolude                        hiding  (try, (<|>), many, option)
import Text.ParserCombinators.Parsec
import Text.Parsec.Prim                 hiding  (try)

symbol :: String -> Parser (String, Maybe Int)
symbol s = parsecMap (\x -> (x, Nothing)) $ string s <* spaces


metas :: [String]
metas = [ "Event"
        , "Site"
        , "Date"
        , "Round"
        , "WhiteElo"
        , "BlackElo"
        , "White"
        , "Black"
        , "Result"
        , "ECO"]

type ChessParser u = ParsecT String u Identity

data Action = Move String String String | Result String String | Unfinished

data Metadata = Metadata String String

data Game = Game [Metadata] [Action]

parseSingleMove :: ChessParser u String
parseSingleMove = many1 (oneOf "abcdefgh12345678NBRQKxOO+-=")


parseMove :: ChessParser u Action
parseMove = do
    n <- many1 digit
    _ <- char '.'
    w <- parseSingleMove
    _ <- space
    b <- option "" parseSingleMove
    return $ Move n w b

parseResult :: ChessParser u Action
parseResult = do
    w <- try (string "1/2") <|> string "1" <|> string "0"
    _ <- char '-'
    b <- try (string "1/2") <|> string "1" <|> string "0"
    return $ Result w b

parseUnfinished :: ChessParser u Action
parseUnfinished = do
    _ <- char '*'
    return Unfinished

parseMoveLine :: ChessParser u [Action]
parseMoveLine = sepBy (try parseMove <|> try parseResult <|> parseUnfinished) (many $ char ' ')

parseMetaLine :: ChessParser () Metadata
parseMetaLine = do
    _ <- symbol "["
    e <- choice $ map (try . string) metas
    spaces
    _ <- symbol "\""
    s <- many $ noneOf "\""
    _ <- string "\"]\n"
    return $ Metadata e s

parseGame :: ChessParser () Game
parseGame = do
    meta <- many1 parseMetaLine
    _ <- newline
    moves <- endBy parseMoveLine newline
    return $ Game meta (concat moves)

parseGames :: ChessParser () [Game]
parseGames = many parseGame

parseFile :: String -> IO (Either ParseError [Game])
parseFile = parseFromFile parseGames


