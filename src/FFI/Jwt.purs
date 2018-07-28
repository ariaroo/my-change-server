module FFI.Jwt (verify) where

import Prelude

import Effect (Effect)
import Foreign (Foreign)

foreign import verify :: String -> String -> (Foreign -> Effect Unit) -> Effect Unit
