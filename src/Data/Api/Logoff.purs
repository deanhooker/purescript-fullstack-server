module Data.Api.Logoff where

import Data.Generic.Rep (class Generic)
import Data.UUID (UUID)
import Foreign.Generic (genericDecode, genericEncode)
import Foreign.Generic.Class (class Decode, class Encode, defaultOptions)

newtype LogoffRequest = LogoffRequest
  { authToken :: UUID }

derive instance genericLogoffRequest :: Generic LogoffRequest _

instance encodeLogoffRequest :: Encode LogoffRequest where
  encode = genericEncode defaultOptions

instance decodeLogoffRequest :: Decode LogoffRequest where
  decode = genericDecode defaultOptions

data LogoffResults = LogoffResultsSuccess | LogoffResultsFailure

derive instance genericLogoffResults :: Generic LogoffResults _

instance encodeLogoffResults :: Encode LogoffResults where
  encode = genericEncode defaultOptions

instance decodeLogoffResults :: Decode LogoffResults where
  decode = genericDecode defaultOptions

newtype LogoffResponse = LogoffResponse LogoffResults

derive instance genericLogoffResponse :: Generic LogoffResponse _

instance encodeLogoffResponse :: Encode LogoffResponse where
  encode = genericEncode defaultOptions

instance decodeLogoffResponse :: Decode LogoffResponse where
  decode = genericDecode defaultOptions
