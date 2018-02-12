
module PGNParser where

import Prelude                                  (String, read)
import Protolude                        hiding  (try, (<|>), many, option)
import Text.ParserCombinators.Parsec
import Text.Parsec.Prim                 hiding  (try)

symbol :: String -> Parser (String, Maybe Int)
symbol s = parsecMap (\x -> (x, Nothing)) $ string s <* spaces


type PGNParser u = ParsecT String u Identity

data MoveText   = Move Int String String
                | GameResult String String
                | Unfinished
                deriving (Eq, Show)

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

data Game = Game Metadata [MoveText]

tagValue :: Tag -> String
tagValue (Tag _ v) = v

isMove :: MoveText -> Bool
isMove Move{} = True
isMove _      = False

tl2m :: Metadata -> [Tag] -> Metadata
tl2m acc []                    = acc
tl2m acc (Tag Event    v : xs) = tl2m acc{ _event    = Tag Event    v } xs
tl2m acc (Tag Site     v : xs) = tl2m acc{ _site     = Tag Site     v } xs
tl2m acc (Tag Date     v : xs) = tl2m acc{ _date     = Tag Date     v } xs
tl2m acc (Tag Round    v : xs) = tl2m acc{ _round    = Tag Round    v } xs
tl2m acc (Tag White    v : xs) = tl2m acc{ _white    = Tag White    v } xs
tl2m acc (Tag Black    v : xs) = tl2m acc{ _black    = Tag Black    v } xs
tl2m acc (Tag Result   v : xs) = tl2m acc{ _result   = Tag Result   v } xs
tl2m acc (Tag WhiteElo v : xs) = tl2m acc{ _whiteElo = Tag WhiteElo v } xs
tl2m acc (Tag BlackElo v : xs) = tl2m acc{ _blackElo = Tag BlackElo v } xs
tl2m acc (Tag ECO      v : xs) = tl2m acc{ _eco      = Tag ECO      v } xs
tl2m acc (Tag Unknown  _ : xs) = tl2m acc                               xs

tl2meta :: [Tag] -> Metadata
tl2meta = tl2m emptyMetadata
    where
        emptyMetadata = Metadata u u u u u u u u u u
        u = Tag Unknown ""

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
    return $ Move (read n :: Int) w b

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
    return $ Game (tl2meta meta) (concat moves)

parseGames :: PGNParser () [Game]
parseGames = many parseGame

parseFile :: String -> IO (Either ParseError [Game])
parseFile = parseFromFile parseGames
