
module Database where

import Protolude

import Database.PostgreSQL.Simple
import Control.Monad
import Control.Applicative

listContents = do
    conn <- connect defaultConnectInfo {
              connectDatabase = "chess"
            }

    -- putStrLn ("2 + 2" :: Text)

    mapM_ print =<< ( query_ conn "select * from content" :: IO [Only Text] )


