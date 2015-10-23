{-# LANGUAGE TemplateHaskell #-}

module ICFP.Config where

import Control.Lens
import Database.PostgreSQL.Simple (Connection)

import Persona (Audience)

data Config = Config
  { _connection :: Connection
  , _audience   :: Audience
  , _filestore  :: FilePath
  }

$(makeLenses ''Config)
