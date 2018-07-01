module Main where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Foldable (oneOf)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Database.Postgres (Query(Query), connect, end, execute, execute_, query, queryOne_, queryValue_, query_, withClient, ClientConfig, ConnectionInfo, connectionInfoFromConfig, defaultPoolConfig, mkPool, release) as Pg
import Database.Postgres.SqlValue (toSql)
import Database.Postgres.Transaction (withTransaction)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Effect.Exception (error, Error)
import Foreign (Foreign)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (decodeJSON, encodeJSON, defaultOptions, genericDecode, genericEncode)
import Foreign.Generic.Types (Options)
import Models (User)
import Node.Encoding (Encoding(..))
import Node.HTTP (Request, Response, listen, createServer, setHeader, requestMethod, requestURL, responseAsStream, requestAsStream, setStatusCode)
import Node.Stream (end, pipe, writeString)
import Partial.Unsafe (unsafeCrashWith)
import Routing (match)
import Routing.Match (Match, bool, int, list, lit, nonempty, num, param, params, str)
import Simple.JSON as JSON


data PostRoutes
  = CustomerLogin String
  | CustomerSignup String

derive instance eqMyRoutes :: Eq PostRoutes
derive instance genericMyRoutes :: Generic PostRoutes _
instance showMyRoutes :: Show PostRoutes where show = genericShow

routing :: Match PostRoutes
routing = oneOf
  [ CustomerLogin <$> (lit "api/v1/customerlogin" *> str)
  , CustomerSignup <$> (lit "api/v1/customersignup" *> str)
  ]


main :: Effect Unit
main = do
  log "Hello sailor!"

  app <- createServer router
  listen app { hostname: "localhost", port: 8080, backlog: Nothing } $ void do
    log "Server setup done!"
    log "Listening on port 8080."


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
        users <- Pg.query_ read' (Pg.Query "select * from users order by first_name desc" :: Pg.Query User) conn
        
        liftEffect $ logShow users

        liftEffect $ setHeader res "Content-Type" "text/plain"
        _ <- liftEffect $ writeString outputStream UTF8 (encodeJSON users) (pure unit)
        liftEffect $ end outputStream (pure unit)
      Pg.end pool
    
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
