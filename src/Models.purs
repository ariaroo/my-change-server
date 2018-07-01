module Models (User) where

import Prelude
import Data.Maybe

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Foreign.Generic.Types (Options)

import Simple.JSON as JSON

newtype User = User {
  id :: Int, 
  user_uuid :: Maybe String,
  first_name :: Maybe String, 
  last_name :: Maybe String,
  email_address :: Maybe String,
  is_email_address_verified :: Maybe Boolean, 
  phone_number :: Maybe String,
  transaction_passcode_hash :: Maybe Boolean, 
  image_url :: Maybe String,
  cloudinary_image_public_id :: Maybe String,
  is_enabled :: Maybe Boolean
}

derive instance genericUser :: Generic User _
derive newtype instance foreignJsonUser :: JSON.ReadForeign User
instance showForeignUser :: Show User
  where show = genericShow

instance decodeUser :: Decode User where
  decode = genericDecode jsonOpts

instance encodeUser :: Encode User where
  encode = genericEncode jsonOpts

jsonOpts :: Options
jsonOpts = defaultOptions { unwrapSingleConstructors = true }
