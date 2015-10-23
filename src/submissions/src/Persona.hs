{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Persona
  ( Assertion
  , Audience
  , PersonaSuccess(..)
  , PersonaFailure(..)
  , verify
  ) where

import Control.Applicative ((<$>))
import Control.Lens
import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson.TH
import Data.Char (toLower)
import Data.Text (Text)
import Network.Wreq

newtype PersonaResponse = PersonaResponse
  { fromPersonaResponse :: Either PersonaFailure PersonaSuccess }
  deriving Show

data PersonaSuccess = PersonaSuccess
  { email    :: Text
  , audience :: Audience
  , expires  :: Int
  , issuer   :: Text
  }
  deriving Show

data PersonaFailure = PersonaFailure
  { reason   :: Text
  }
  deriving Show

data PersonaStatus = Okay | Failure
  deriving (Eq, Show)

newtype Assertion = Assertion Text
  deriving (Eq, Ord, Show, FormValue, FromJSON, ToJSON)

type Audience = Text

$(deriveJSON defaultOptions ''PersonaSuccess)
$(deriveJSON defaultOptions ''PersonaFailure)
$(deriveJSON defaultOptions { constructorTagModifier = map toLower } ''PersonaStatus)

instance FromJSON PersonaResponse where
  parseJSON input@(Object v) = do
    status <- v .: "status"
    case status of
      Okay    -> PersonaResponse . Right <$> parseJSON input
      Failure -> PersonaResponse . Left  <$> parseJSON input
  parseJSON _ = mzero

verifier :: String
verifier = "https://verifier.login.persona.org/verify"

verify :: Assertion -> Audience -> IO (Either PersonaFailure PersonaSuccess)
verify assertion aud = do
  res <- asJSON =<< post verifier [ "assertion" := assertion
                                  , "audience"  := aud
                                  ]
  return $ fromPersonaResponse (res ^. responseBody)
