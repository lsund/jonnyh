
module JonnyH.Database.Common where

import           Database.PostgreSQL.Simple

import           Protolude
import           PGNParser.Data.Move


makeConnection :: IO Connection
makeConnection = connect defaultConnectInfo { connectDatabase = "chess" }


