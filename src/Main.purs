module Main where

import Prelude

import Control.Plus (empty)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (oneOf)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.String (stripPrefix)
import Data.String.CodeUnits (drop)
import Database.Postgres (ClientConfig, ConnectionInfo, Query(Query), connectionInfoFromConfig, defaultPoolConfig, end, mkPool, query_, withClient) as Pg
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error, Error)
import Foreign (Foreign)
import Foreign.Generic (encodeJSON)
import Models (User)
import Node.Encoding (Encoding(..))
import Node.HTTP (Request, Response, listen, createServer, setHeader, requestMethod, requestURL, responseAsStream, requestAsStream, setStatusCode)
import Node.Stream (Stream, Write, Writable, end, pipe, writeString)
import Partial.Unsafe (unsafeCrashWith)
import Routing (match)
import Routing.Match (Match, lit, nonempty, num, str, params)
import Simple.JSON as JSON


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



data PostRoutes
  = CustomerLogin Unit
  | CustomerSignup Unit

derive instance eqMyRoutes :: Eq PostRoutes
derive instance genericMyRoutes :: Generic PostRoutes _
instance showMyRoutes :: Show PostRoutes where 
  show = genericShow

routing :: Match PostRoutes
routing = oneOf
  [ CustomerLogin <$> (lit "api" *> lit "v1" *> lit "customerlogin")
  , CustomerSignup <$> (lit "api" *> lit "v1" *> lit "customersignup")
  ]


handleCustomerLogin :: forall a. Request -> Response -> (Writable a) -> Effect Unit
handleCustomerLogin req res outStream = do
  pool <- Pg.mkPool connectionInfo

  launchAff_ $ Pg.withClient pool $ \conn -> do
    users <- Pg.query_ read' (Pg.Query "select * from users order by first_name desc" :: Pg.Query User) conn
    
    liftEffect $ log $ encodeJSON users

    liftEffect $ setStatusCode res 200
    liftEffect $ setHeader res "Content-Type" "text/plain"
    
    _ <- liftEffect $ writeString outStream UTF8 (encodeJSON users) (pure unit)
    liftEffect $ end outStream (pure unit)
  
  Pg.end pool

handleCustomerSignup :: forall a. Request -> Response -> (Writable a) -> Effect Unit
handleCustomerSignup req res outStream = do
  pool <- Pg.mkPool connectionInfo

  launchAff_ $ Pg.withClient pool $ \conn -> do
    users <- Pg.query_ read' (Pg.Query "select * from users order by first_name desc" :: Pg.Query User) conn
    
    liftEffect $ log $ encodeJSON users

    liftEffect $ setStatusCode res 200
    liftEffect $ setHeader res "Content-Type" "text/plain"
    
    _ <- liftEffect $ writeString outStream UTF8 "Foob return" (pure unit)
    liftEffect $ end outStream (pure unit)
  
  Pg.end pool


router :: Request -> Response -> Effect Unit
router req res = do
  log $ "Request path: " <> show (match routing $ ((drop 1) <<< requestURL) req)

  let inputStream  = requestAsStream req
      outputStream = responseAsStream res

  case requestMethod req of
    "POST" -> do
      case match routing $ ((drop 1) <<< requestURL) req of 
        Right (CustomerLogin _) -> 
          handleCustomerLogin req res outputStream
        Right (CustomerSignup _) -> 
          handleCustomerSignup req res outputStream

        _ -> void $ pipe inputStream outputStream
    _ -> unsafeCrashWith "Unexpected HTTP method"

main :: Effect Unit
main = do
  app <- createServer router
  listen app { hostname: "localhost", port: 8080, backlog: Nothing } $ do
    log "Server listening on port 8080."
