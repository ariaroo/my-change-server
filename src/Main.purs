module Main where

import Prelude

import Control.Monad.Except (runExcept)
import Control.Plus (empty)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (oneOf)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Generic.Rep.Show (genericShow)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.String (stripPrefix)
import Data.String.CodeUnits (drop)
import Database.Postgres (ClientConfig, ConnectionInfo, Query(Query), connectionInfoFromConfig, Pool, defaultPoolConfig, end, mkPool, query_, withClient) as Pg
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Effect.Exception (error, Error)
import Foreign (Foreign, unsafeFromForeign)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Foreign.Generic (encodeJSON)
import Foreign.Generic.Types (Options)
import Models (User)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.HTTP (Request, Response, listen, createServer, setHeader, requestMethod, requestURL, responseAsStream, requestAsStream, setStatusCode)
import Node.Stream (Stream, Write, Writable, end, pipe, writeString)
import Partial.Unsafe (unsafeCrashWith)
import Routing (match)
import Routing.Match (Match, lit, nonempty, num, str, params)
import Simple.JSON as JSON

import FFI.UUID as UUID
import FFI.BCrypt as BCrypt


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


handleCustomerLogin :: Request -> Response -> Pg.Pool -> Effect Unit
handleCustomerLogin req res pool = do
  launchAff_ $ Pg.withClient pool $ \conn -> do
    users <- Pg.query_ read' (Pg.Query "select * from users order by first_name desc" :: Pg.Query User) conn
    
    liftEffect $ do
      log $ encodeJSON users

      setStatusCode res 200
      setHeader res "Content-Type" "text/plain"

      let oStream = responseAsStream res    
      _ <- writeString oStream UTF8 (encodeJSON users) (pure unit)

      end oStream (pure unit)

handleCustomerSignup :: Request -> Response -> Pg.Pool -> Effect Unit
handleCustomerSignup req res pool = do
  launchAff_ $ Pg.withClient pool $ \conn -> do
    users <- Pg.query_ read' (Pg.Query "select * from users order by first_name desc" :: Pg.Query User) conn
    
    liftEffect $ do 
      log $ encodeJSON users

      setStatusCode res 200
      setHeader res "Content-Type" "text/plain"

      let oStream = responseAsStream res    
      _ <- writeString oStream UTF8 "Foob return" (pure unit)

      end oStream (pure unit)


router :: Request -> Response -> Pg.Pool -> Effect Unit
router req res pool = do
  log $ "Request path: " <> show (match routing $ ((drop 1) <<< requestURL) req)

  case requestMethod req of
    "POST" -> do
      case match routing $ ((drop 1) <<< requestURL) req of 
        Right (CustomerLogin _) -> 
          handleCustomerLogin req res pool
        Right (CustomerSignup _) -> 
          handleCustomerSignup req res pool

        _ -> void $ pipe (requestAsStream req) (responseAsStream res)
    _ -> unsafeCrashWith "Unexpected HTTP method"


getDbConfig :: String -> Either String Pg.ClientConfig
getDbConfig s = lmap show (JSON.readJSON s)

main :: Effect Unit
main = launchAff_ $ do 
  contents <- readTextFile UTF8 "./config/dbDev.json"

  liftEffect $ do
    log $ "New uuid: " <> (UUID.new)

    let newPasswordHash = BCrypt.getPasswordHash "asdffdsa"
    log $ "Password hash: " <> newPasswordHash
    let isPasswordCorrect = BCrypt.isPasswordCorrect "asdffdsa" newPasswordHash
    logShow isPasswordCorrect

    -- log ("\nconfig contents: " <> contents)

    case getDbConfig contents of
      Left error -> do
        log error
      Right clientConfig -> do
        dbPool <- Pg.mkPool $ Pg.connectionInfoFromConfig clientConfig Pg.defaultPoolConfig

        app <- createServer (\req res -> router req res dbPool)
        listen app { hostname: "localhost", port: 8080, backlog: Nothing } $ do
          log "Server listening on port 8080."
