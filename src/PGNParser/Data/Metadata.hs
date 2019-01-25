module PGNParser.Data.Metadata where

import Prelude (String)

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


tagValue :: Tag -> String
tagValue (Tag _ v) = v


