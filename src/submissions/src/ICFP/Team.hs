{-# LANGUAGE TemplateHaskell #-}

module ICFP.Team where

import Control.Applicative
import Control.Lens
import Data.Aeson.TH
import Data.Text (Text)
import Data.Time.Clock
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow, field)
import Database.PostgreSQL.Simple.ToRow (ToRow, toRow)
import Database.PostgreSQL.Simple.ToField (toField)

import ICFP.Internal (stripRecordPrefix)
import ICFP.User

data Team = Team
  { _teamId        :: Maybe Int
  , _teamName      :: Text
  , _teamCreatedAt :: Maybe UTCTime
  , _teamMembers   :: [Email]
  }

$(deriveJSON defaultOptions { fieldLabelModifier = stripRecordPrefix } ''Team)
$(makeLenses ''Team)

instance FromRow Team where
  fromRow = Team <$> field <*> field <*> field <*> pure []

instance ToRow Team where
  toRow t = [toField (t^.teamName), toField (t^.teamCreatedAt)]
