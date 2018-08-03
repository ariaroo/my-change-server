module Handlers.Customer (login, signup) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Array ((!!), head, unsafeIndex)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Generic.Rep.Show (genericShow)
import Database.Postgres (ClientConfig, ConnectionInfo, Query(Query), connectionInfoFromConfig, Pool, defaultPoolConfig, end, mkPool, query_, withClient) as Pg
import Db.Users as DbUsers
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Effect.Exception (error, Error)
import FFI.Jwt as Jwt
import Foreign (Foreign, unsafeFromForeign)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Foreign.Generic (encodeJSON)
import Foreign.Generic.Types (Options)
import Models (User, getUserJwtPayload)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.HTTP (Request, Response, listen, createServer, setHeader, requestMethod, requestURL, responseAsStream, requestAsStream, setStatusCode)
import Node.Stream (Stream, Write, Writable, end, pipe, writeString)
import Utils as Utils

import FFI.Jwt as Jwt


login :: Request -> Response -> Pg.Pool -> Effect Unit
login req res pool = do
  launchAff_ $ Pg.withClient pool $ \conn -> do
    let query = DbUsers.allUsersQuery
    liftEffect $ log "Query: " <> log query

    users <- Pg.query_ Utils.read' (Pg.Query query :: Pg.Query User) conn
    let firstUser = users !! 0

    case firstUser of
      Just user -> 
        liftEffect $ do
          -- log $ encodeJSON users
      
          log $ "Jwt signature: " <> Jwt.sign (getUserJwtPayload user) "wwlwlwwll"

          setStatusCode res 200
          setHeader res "Content-Type" "text/plain"

          let oStream = responseAsStream res    
          _ <- writeString oStream UTF8 (encodeJSON users) (pure unit)

          end oStream (pure unit)
      Nothing -> do
        liftEffect $ do
          setStatusCode res 200
          setHeader res "Content-Type" "text/plain"

          let oStream = responseAsStream res    
          _ <- writeString oStream UTF8 "No users found" (pure unit)

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
