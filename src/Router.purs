module Router (route) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Generic.Rep.Show (genericShow)
import Handlers.Customer as CustomerHandlers
import Partial.Unsafe (unsafeCrashWith)
import Routing (match)
import Routing.Match (Match, lit, nonempty, num, str, params)
import Data.Foldable (oneOf)

import Node.HTTP (Request, Response, listen, createServer, setHeader, requestMethod, requestURL, responseAsStream, requestAsStream, setStatusCode)
import Node.Stream (Stream, Write, Writable, end, pipe, writeString)

import Database.Postgres (ClientConfig, ConnectionInfo, Query(Query), connectionInfoFromConfig, Pool, defaultPoolConfig, end, mkPool, query_, withClient) as Pg

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)

import Data.Either (Either(..))

import Utils as Utils


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


route :: Request -> Response -> Pg.Pool -> Effect Unit
route req res pool = do
  let requestRoute = match routing $ (Utils.cleanReqUrl <<< requestURL) req
  log $ "Request path: " <> show requestRoute

  case requestMethod req of
    "POST" -> do
      case requestRoute of 
        Right (CustomerLogin _) -> 
          CustomerHandlers.login req res pool
        Right (CustomerSignup _) -> 
          CustomerHandlers.signup req res pool

        _ -> void $ pipe (requestAsStream req) (responseAsStream res)
    _ -> unsafeCrashWith "Unexpected HTTP method"
