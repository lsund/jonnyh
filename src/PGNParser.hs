
module PGNParser where

import Prelude                                  (String)
import Protolude                        hiding  (try, (<|>), many, option)
import Text.ParserCombinators.Parsec
import Text.Parsec.Prim                 hiding  (try)

symbol :: String -> Parser (String, Maybe Int)
symbol s = parsecMap (\x -> (x, Nothing)) $ string s <* spaces


type PGNParser u = ParsecT String u Identity

data MoveText = Move String String String | GameResult String String | Unfinished

data TagKey = Event
            | Site
            | Date
            | Round
            | WhiteElo
            | BlackElo
            | White
            | Black
            | Result
            | ECO
            | Unknown

data Tag = Tag TagKey String

data Metadata = Metadata { _event :: Tag
                         , _site :: Tag
                         , _date :: Tag
                         , _round :: Tag
                         , _white :: Tag
                         , _black :: Tag
                         , _result :: Tag
                         , _whiteElo :: Tag
                         , _blackElo :: Tag
                         , _eco :: Tag
                         }

data Game = Game [Tag] [MoveText]

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

tagValue :: Tag -> String
tagValue (Tag _ v) = v

stringToTagKey :: String -> TagKey
stringToTagKey "Event"    = Event
stringToTagKey "Site"     = Site
stringToTagKey "Date"     = Date
stringToTagKey "Round"    = Round
stringToTagKey "WhiteElo" = WhiteElo
stringToTagKey "BlackElo" = BlackElo
stringToTagKey "White"    = White
stringToTagKey "Black"    = Black
stringToTagKey "Result"   = Result
stringToTagKey "ECO"      = ECO
stringToTagKey _          = Unknown


parseSingleMove :: PGNParser u String
parseSingleMove = many1 (oneOf "abcdefgh12345678NBRQKxOO+-=")


parseMove :: PGNParser u MoveText
parseMove = do
    n <- many1 digit
    _ <- char '.'
    w <- parseSingleMove
    _ <- space
    b <- option "" parseSingleMove
    return $ Move n w b

parseResult :: PGNParser u MoveText
parseResult = do
    w <- try (string "1/2") <|> string "1" <|> string "0"
    _ <- char '-'
    b <- try (string "1/2") <|> string "1" <|> string "0"
    return $ GameResult w b

parseUnfinished :: PGNParser u MoveText
parseUnfinished = do
    _ <- char '*'
    return Unfinished

parseMoveLine :: PGNParser u [MoveText]
parseMoveLine = sepBy (try parseMove <|> try parseResult <|> parseUnfinished) (many $ char ' ')

parseMetaLine :: PGNParser () Tag
parseMetaLine = do
    _ <- symbol "["
    e <- choice $ map (try . string) metaLabels
    spaces
    _ <- symbol "\""
    s <- many $ noneOf "\""
    _ <- string "\"]\n"
    return $ Tag (stringToTagKey e) s

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
