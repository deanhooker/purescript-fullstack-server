module Data.Api.Logon where

import Data.Generic.Rep (class Generic)
import Data.UUID (UUID)
import Foreign.Generic (genericDecode, genericEncode)
import Foreign.Generic.Class (class Decode, class Encode, defaultOptions)

-- TODO: Why use newtype instead of type?
newtype LogonRequest = LogonRequest
  { userName :: String
  , password :: String
  }

derive instance genericLogonRequest :: Generic LogonRequest _

instance encodeLogonRequest :: Encode LogonRequest where
  encode = genericEncode defaultOptions

instance decodeLogonRequest :: Decode LogonRequest where
  decode = genericDecode defaultOptions

data LogonResults
  = LogonResultsSuccess
    { authToken :: UUID
    , admin :: Boolean
    , mustChangePassword :: Boolean
    }
  | LogonResultsFailure

derive instance genericLogonResults :: Generic LogonResults _

instance encodeLogonResults :: Encode LogonResults where
  encode = genericEncode defaultOptions

instance decodeLogonResults :: Decode LogonResults where
  decode = genericDecode defaultOptions

newtype LogonResponse = LogonResponse LogonResults

derive instance genericLogonResponse :: Generic LogonResponse _

instance encodeJSONLogonResponse :: Encode LogonResponse where
  encode = genericEncode defaultOptions

instance decodeJSONLogonResponse :: Decode LogonResponse where
  decode = genericDecode defaultOptions
