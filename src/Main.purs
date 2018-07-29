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
import Node.HTTP (createServer, listen)
import Node.Stream (Stream, Write, Writable, end, pipe, writeString)

import Utils as Utils

import FFI.UUID as UUID
import FFI.BCrypt as BCrypt

import Router as Router


main :: Effect Unit
main = launchAff_ $ do 
  contents <- readTextFile UTF8 "./config/dbDev.json"

  let uuid = UUID.new
  let newPasswordHash = BCrypt.getPasswordHash "asdffdsa"
  let isPasswordCorrect = BCrypt.isPasswordCorrect "asdffdsa" newPasswordHash

  liftEffect $ do
    log $ "New uuid: " <> uuid <> "\n"

    log $ "Password hash: " <> newPasswordHash
    log $ "Is password correct: " <> show isPasswordCorrect

    log ("\nconfig contents: " <> contents)

    case Utils.getDbConfig contents of
      Left error -> do
        log error
      Right clientConfig -> do
        dbPool <- Pg.mkPool $ Pg.connectionInfoFromConfig clientConfig Pg.defaultPoolConfig

        app <- createServer (\req res -> Router.route req res dbPool)
        listen app { hostname: "localhost", port: 8080, backlog: Nothing } $ do
          log "Server listening on port 8080."
