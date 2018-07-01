module Main where

import Prelude
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Effect.Exception (error, Error)

import Control.Monad.Error.Class (throwError)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Database.Postgres (Query(Query), connect, end, execute, execute_, query, queryOne_, queryValue_, query_, withClient, ClientConfig, ConnectionInfo, connectionInfoFromConfig, defaultPoolConfig, mkPool, release) as Pg
import Database.Postgres.SqlValue (toSql)
import Database.Postgres.Transaction (withTransaction)

import Effect.Aff (Aff, launchAff_)

import Foreign (Foreign)
import Node.Encoding (Encoding(..))
import Node.HTTP (Request, Response, listen, createServer, setHeader, requestMethod, requestURL, responseAsStream, requestAsStream, setStatusCode)
import Node.Stream (end, pipe, writeString)
import Partial.Unsafe (unsafeCrashWith)
import Simple.JSON as JSON

import Models (Users)


main :: Effect Unit
main = do
  log "Hello sailor!"

  app <- createServer router
  listen app { hostname: "localhost", port: 8080, backlog: Nothing } $ void do
    log "Server setup done!"
    log "Listening on port 8080."


-- type User = { id :: Int, name :: String }


router :: Request -> Response -> Effect Unit
router req res = do
  setStatusCode res 200

  let inputStream  = requestAsStream req
      outputStream = responseAsStream res
  log (requestMethod req <> " " <> requestURL req)

  case requestMethod req of
    "GET" -> do
      pool <- Pg.mkPool connectionInfo

      launchAff_ $ Pg.withClient pool $ \conn -> do
        users <- Pg.query_ read' (Pg.Query "select * from users order by first_name desc" :: Pg.Query Users) conn
        
        liftEffect $ logShow users

      Pg.end pool
      
      let html = foldMap (_ <> "\n")
            [ "<form method='POST' action='/'>"
            , "  <input name='text' type='text'>"
            , "  <input type='submit'>"
            , "</form>"
            ]
      setHeader res "Content-Type" "text/html"
      
      _ <- writeString outputStream UTF8 html (pure unit)
      end outputStream (pure unit)
    "POST" -> void $ pipe inputStream outputStream
    _ -> unsafeCrashWith "Unexpected HTTP method"


clientConfig :: Pg.ClientConfig
clientConfig =
  { host: "localhost"
  , database: "mychangedb"
  , port: 5432
  , user: "postgres"
  , password: "asdffdsa"
  , ssl: false
  }

connectionInfo :: Pg.ConnectionInfo
connectionInfo = Pg.connectionInfoFromConfig clientConfig Pg.defaultPoolConfig

read' :: forall a. JSON.ReadForeign a => Foreign -> Either Error a
read' = lmap (error <<< show) <<< JSON.read
