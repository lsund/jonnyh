module JonnyH.Database.Query where

import           Data.List                  (foldr1, last)
import           Data.Set                   hiding (map)
import           Database.PostgreSQL.Simple
import           Protolude                  hiding (toList)
import           System.Random

import           JonnyH.Color
import           JonnyH.Database.Common
import           JonnyH.Types
import           JonnyH.Util
-- import           PGNParser.Data.Move

import           Prelude                    (String)

-- type PGNMove =  PGNParser.Data.Move.Move

-- maxId :: Connection -> IO Int
-- maxId conn = do
--     (Only x : _) <- query_ conn "select max(id) from game" :: IO [Only Int]
--     return x

-- serializeTurn :: Turn -> (Int, String, String)
-- serializeTurn (n, ((wp, ws), (bp, bs))) =
--     (n, show wp <> show ws, show bp <> show bs)

-- queryTurn :: Connection -> Query -> Turn -> IO [Only Int]
-- queryTurn conn q turn = query conn q $ serializeTurn turn

-- gamesWithTurn :: Connection -> Turn -> IO (Set Int)
-- gamesWithTurn conn turn = do
--     let q = "select gameid from move where movenumber=? and white=? and black=?"
--     xs <- queryTurn conn q turn
--     return $ fromList $ map fromOnly xs


-- gamesWithTurnSequence :: Connection -> [Turn] -> IO (Set Int)
-- gamesWithTurnSequence conn turns = do
--     games <- mapM (gamesWithTurn conn) turns
--     return $ foldr1 intersection games

-- randomMove :: Double -> [(Text, Int)] -> Maybe (Text, Double)
-- randomMove _ [] = Nothing
-- randomMove r xs =
--     case dropWhile (\(_, y) -> r > y) (applyToSnds normalize xs) of
--         []    -> Nothing
--         y : _ -> Just y

-- doQuery :: Connection -> Set Int -> Maybe Int -> Color -> IO (Maybe (Text, Double))
-- doQuery conn games moveNumber color = do
--     let q = case color of
--                 White ->"select white,count(white) \
--                         \from move \
--                         \where gameid in ? and movenumber=? group by white"
--                 Black ->"select black,count(black) \
--                         \from move \
--                         \where gameid in ? and movenumber=? group by black"
--         params = (In (toList games), moveNumber)
--     res <- query conn q params :: IO [(Text, Int)]
--     r <- randomRIO (0.0, 1.0) :: IO Double
--     return $ randomMove r res

-- nextTurnNumber :: [Turn] -> Maybe Int
-- nextTurnNumber =  (succ . last  <$>) . maybeEmptyList . map moveNumber
--     where moveNumber (n, _) = n

-- response :: [Turn] -> Color -> IO (Maybe (Text, Double))
-- response turns color = do
--     conn <- makeConnection
--     games <- gamesWithTurnSequence conn turns
--     doQuery conn games (nextTurnNumber turns) color
