module Api.QueryUsers where

import Data.Generic.Rep (class Generic)
import Data.UUID (UUID)
import Entity.User (User)
import Foreign.Generic (genericDecode, genericEncode)
import Foreign.Generic.Class (class Decode, class Encode, defaultOptions)

newtype QueryUsersRequest = QueryUsersRequest { authToken :: UUID }

data FailureReason
  = NotAuthorized
  | NotAuthenticated

derive instance genericFailureReason :: Generic FailureReason _

instance encodeFailureReason :: Encode FailureReason where
  encode = genericEncode defaultOptions

instance decodeFailureReason :: Decode FailureReason where
  decode = genericDecode defaultOptions

data QueryUsersResults
  = QueryUsersResultsSuccess { users  :: Array User }
  | QueryUsersResultsFailure { reason :: FailureReason }

derive instance genericQueryUsersResults :: Generic QueryUsersResults _

instance encodeQueryUsersResults :: Encode QueryUsersResults where
  encode = genericEncode defaultOptions

instance decodeQueryUsersResults :: Decode QueryUsersResults where
  decode = genericDecode defaultOptions

newtype QueryUsersResponse = QueryUsersResponse QueryUsersResults

derive instance genericQueryUsersResponse :: Generic QueryUsersResponse _

instance encodeQueryUsersResponse :: Encode QueryUsersResponse where
  encode = genericEncode defaultOptions

instance decodeQueryUsersResponse :: Decode QueryUsersResponse where
  decode = genericDecode defaultOptions
