module Models (Users) where

import Data.Maybe


type Users = {
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