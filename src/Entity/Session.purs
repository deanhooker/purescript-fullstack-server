module Entity.Session where

import Data.UUID (UUID)

newtype Session = Session
  { authToken :: UUID
  , userName :: String
  , lastTime :: Number
  }
