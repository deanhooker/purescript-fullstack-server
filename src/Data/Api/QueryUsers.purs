module Data.Api.QueryUsers where

import Data.Generic.Rep (class Generic)
import Data.UUID (UUID)
import Entity.User (User)
import Foreign.Generic (genericDecode, genericEncode)
import Foreign.Generic.Class (class Decode, class Encode, defaultOptions)

newtype QueryUsersRequest = QueryUsersRequest { authToken :: UUID }

derive instance genericQueryusersRequest :: Generic QueryUsersRequest _

instance encodeQueryusersRequest :: Encode QueryUsersRequest where
  encode = genericEncode defaultOptions

instance decodeQueryusersRequest :: Decode QueryUsersRequest where
  decode = genericDecode defaultOptions

data QueryUsersFailureReason
  = NotAuthorized
  | NotAuthenticated

derive instance genericQueryUsersFailureReason :: Generic QueryUsersFailureReason _

instance encodeQueryUsersFailureReason :: Encode QueryUsersFailureReason where
  encode = genericEncode defaultOptions

instance decodeQueryUsersFailureReason :: Decode QueryUsersFailureReason where
  decode = genericDecode defaultOptions

data QueryUsersResults
  = QueryUsersResultsSuccess { users  :: Array User }
  | QueryUsersResultsFailure { reason :: QueryUsersFailureReason }

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
