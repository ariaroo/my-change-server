module Handlers.Customer (login, signup) where

import Prelude

import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.HTTP (Request, Response, listen, createServer, setHeader, requestMethod, requestURL, responseAsStream, requestAsStream, setStatusCode)
import Node.Stream (Stream, Write, Writable, end, pipe, writeString)

import Database.Postgres (ClientConfig, ConnectionInfo, Query(Query), connectionInfoFromConfig, Pool, defaultPoolConfig, end, mkPool, query_, withClient) as Pg

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Effect.Exception (error, Error)

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Generic.Rep.Show (genericShow)

import Foreign (Foreign, unsafeFromForeign)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Foreign.Generic (encodeJSON)
import Foreign.Generic.Types (Options)


import Models (User)
import Utils as Utils
import Db.Users as DbUsers


login :: Request -> Response -> Pg.Pool -> Effect Unit
login req res pool = do
  launchAff_ $ Pg.withClient pool $ \conn -> do
    let query = DbUsers.allUsersQuery
    liftEffect $ log "Query: " <> log query

    users <- Pg.query_ Utils.read' (Pg.Query DbUsers.allUsersQuery :: Pg.Query User) conn
    
    liftEffect $ do
      log $ encodeJSON users

      setStatusCode res 200
      setHeader res "Content-Type" "text/plain"

      let oStream = responseAsStream res    
      _ <- writeString oStream UTF8 (encodeJSON users) (pure unit)

      end oStream (pure unit)

signup :: Request -> Response -> Pg.Pool -> Effect Unit
signup req res pool = do
  launchAff_ $ Pg.withClient pool $ \conn -> do
    users <- Pg.query_ Utils.read' (Pg.Query "select * from users order by first_name desc" :: Pg.Query User) conn
    
    liftEffect $ do 
      log $ encodeJSON users

      setStatusCode res 200
      setHeader res "Content-Type" "text/plain"

      let oStream = responseAsStream res    
      _ <- writeString oStream UTF8 "Foob return" (pure unit)

      end oStream (pure unit)
