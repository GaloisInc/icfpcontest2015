{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}

module ICFP.Session
 ( CreateSession
 , Session(..)
 , createSession
 ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import Data.ByteString.Lazy (ByteString, fromChunks)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import Persona (Assertion, PersonaFailure(..), PersonaSuccess(..), verify)
import Servant

import           ICFP.Database
import           ICFP.Config
import qualified ICFP.Config as Config
import           ICFP.User

type CreateSession = ReqBody '[JSON] Login :> Post '[JSON] Session

data Login = Login { assertion :: Assertion }
  deriving (Eq, Generic, Ord, Show)

data Session = Session { apiToken :: ApiToken }
  deriving (Eq, Generic, Ord, Show)

type Servant a = forall m. (Applicative m, MonadIO m, MonadError ServantErr m) => m a

instance FromJSON Login
instance ToJSON   Session

createSession :: Config -> Login -> Servant Session
createSession config login = do
  personaResp <- liftIO $ verify (assertion login) (config^.Config.audience)
  case personaResp of
    Left  resp -> throwError err401 { errBody = message (reason resp) }
    Right resp -> do
      user <- liftIO $ findOrCreateUserByEmail (config^.connection) (Persona.email resp)
      return $ Session (user ^. userApiToken)

message :: Text -> ByteString
message = fromChunks . return . encodeUtf8
