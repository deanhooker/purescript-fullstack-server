module Manager.Session where

import Prelude

import Data.JSDate (getTime, now)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.UUID (UUID, genUUID)
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Entity.Session (Session(..))

type Sessions = Map UUID Session

startUp :: Aff (AVar Sessions)
startUp = AVar.new Map.empty

shutdown :: AVar Sessions -> Aff Unit
shutdown = void <<< AVar.take

verifySession :: AVar Sessions -> UUID -> Aff (Maybe Session)
verifySession sessionsAVar authToken = do
  expireSessions sessionsAVar
  sessions <- AVar.take sessionsAVar
  now <- getTime <$> liftEffect now
  let Tuple newSessions newSession =
        case Map.lookup authToken sessions of
          Nothing -> Tuple sessions Nothing
          Just (Session session) -> do
            let newSession = Session $ session { lastTime = now }
                sessions' = Map.insert authToken newSession sessions
            Tuple sessions' (Just newSession)
  AVar.put newSessions sessionsAVar
  pure newSession

createSession :: AVar Sessions -> String -> Aff UUID
createSession sessionsAVar userName = do
  lastTime <- getTime <$> liftEffect now
  authToken <- liftEffect genUUID
  sessions <- AVar.take sessionsAVar
  let newSession = Session { authToken, userName, lastTime }
      sessions' = Map.insert authToken newSession sessions
  AVar.put sessions' sessionsAVar
  pure authToken

expireSessions :: AVar Sessions -> Aff Unit
expireSessions sessionsAVar = do
  now <- getTime <$> liftEffect now
  sessions <- AVar.take sessionsAVar
  let sessions' = Map.filter
                  (\(Session { lastTime }) ->  now - lastTime > sessionTimeout ) sessions
  AVar.put sessions' sessionsAVar

sessionTimeout :: Number
sessionTimeout = 4.0 * 60.0 * 60.0 * 1000.0 -- 4 hours
