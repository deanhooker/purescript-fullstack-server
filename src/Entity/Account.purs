module Entity.Account where

import Entity.User (UserRow)

newtype Account = Account (Record (UserRow ( passwordHash :: String )))
