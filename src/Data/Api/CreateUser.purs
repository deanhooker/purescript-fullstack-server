module CreateUser where

import Data.Generic.Rep (class Generic)
import Data.UUID (UUID)
import Entity.User (UserRow)
import Foreign.Generic (genericDecode, genericEncode)
import Foreign.Generic.Class (class Decode, class Encode, defaultOptions)

newtype CreateUserRequest
  = CreateUserRequest
    { authToken :: UUID
    , user :: Record (UserRow ( password :: String ))
    }

derive instance genericCreateUserRequest :: Generic CreateUserRequest _

instance encodeCreateUserRequest :: Encode CreateUserRequest where
  encode = genericEncode defaultOptions

instance decodeCreateUserRequest :: Decode CreateUserRequest where
  decode = genericDecode defaultOptions

data FailureReason
  = AlreadyExists
  | NotAuthorized
  | NotAuthenticated

derive instance genericFailureReason :: Generic FailureReason _

instance encodeFailureReason :: Encode FailureReason where
  encode = genericEncode defaultOptions

instance decodeFailureReason :: Decode FailureReason where
  decode = genericDecode defaultOptions

data CreateUserResults
  = CreateUserResultsSuccess
  | CreateUserResultsFailure { reason :: FailureReason }

derive instance genericCreateUserResults :: Generic CreateUserResults _

instance encodeCreateUserResults :: Encode CreateUserResults where
  encode = genericEncode defaultOptions

instance decodeCreateUserResults :: Decode CreateUserResults where
  decode = genericDecode defaultOptions

newtype CreateUserResponse = CreateUserResponse CreateUserResults

derive instance genericCreateUserResponse :: Generic CreateUserResponse _

instance encodeCreateUserResponse :: Encode CreateUserResponse where
  encode = genericEncode defaultOptions

instance decodeCreateUserResponse :: Decode CreateUserResponse where
  decode = genericDecode defaultOptions
