{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module ICFP.User where

import Control.Applicative ((<$>), (<*>))
import Control.Lens
import Data.Aeson.TH
import Data.Text
import Data.Time.Clock
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow, field)
import Database.PostgreSQL.Simple.ToRow (ToRow, toRow)
import Database.PostgreSQL.Simple.ToField (toField)
import GHC.Generics

import ICFP.Internal (stripRecordPrefix)

type Email    = Text
type ApiToken = Text

data User = User
  { _userId        :: Maybe Int
  , _userEmail     :: Email
  , _userCreatedAt :: UTCTime
  , _userApiToken  :: ApiToken
  }
  deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions { fieldLabelModifier = stripRecordPrefix } ''User)
$(makeLenses ''User)

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field

instance ToRow User where
  toRow u = [toField (u^.userEmail), toField (u^.userCreatedAt), toField (u^.userApiToken)]
