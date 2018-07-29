module Utils (read', cleanReqUrl, getDbConfig) where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either)
import Effect.Exception (error, Error)
import Foreign (Foreign)
import Simple.JSON as JSON

import Data.String.CodeUnits (drop)
import Database.Postgres (ClientConfig, ConnectionInfo, Query(Query), connectionInfoFromConfig, Pool, defaultPoolConfig, end, mkPool, query_, withClient) as Pg


read' :: forall a. JSON.ReadForeign a => Foreign -> Either Error a
read' = lmap (error <<< show) <<< JSON.read

cleanReqUrl :: String -> String
cleanReqUrl requestURL = drop 1 requestURL

getDbConfig :: String -> Either String Pg.ClientConfig
getDbConfig s = lmap show (JSON.readJSON s)
