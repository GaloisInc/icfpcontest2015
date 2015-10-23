{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module ICFP.Solution where

import Control.Applicative
import Control.Lens
import Data.Aeson.TH
import Data.Time.Clock
import Data.Text (Text)
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow, field)
import Database.PostgreSQL.Simple.ToRow (ToRow, toRow)
import Database.PostgreSQL.Simple.ToField (toField)
import GHC.Generics

import ICFP.Internal (stripRecordPrefix)

data Solution = Solution
  { _solutionTeamId    :: Int
  , _solutionAuthorId  :: Int
  , _solutionCreatedAt :: UTCTime
  , _solutionProblemId :: Int
  , _solutionSeed      :: Int
  , _solutionScore     :: Maybe Int
  , _solutionPowerScore :: Maybe Int
  , _solutionSolution  :: Text
  , _solutionTag       :: Text
  }
  deriving (Eq, Show, Generic)

data UploadedSolution = UploadedSolution
  { _uploadedProblemId :: Int
  , _uploadedSeed      :: Int
  , _uploadedSolution  :: Text
  , _uploadedTag       :: Maybe Text
  }
  deriving (Eq, Show, Generic)

$(makeLenses ''Solution)
$(makeLenses ''UploadedSolution)

$(deriveJSON defaultOptions { fieldLabelModifier = stripRecordPrefix } ''Solution)
$(deriveJSON defaultOptions { fieldLabelModifier = stripRecordPrefix } ''UploadedSolution)

instance FromRow Solution where
  fromRow = Solution <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Solution where
  toRow s = [ toField (s^.solutionTeamId)
            , toField (s^.solutionAuthorId)
            , toField (s^.solutionCreatedAt)
            , toField (s^.solutionProblemId)
            , toField (s^.solutionSeed)
            , toField (s^.solutionScore)
            , toField (s^.solutionPowerScore)
            , toField (s^.solutionSolution)
            , toField (s^.solutionTag)
            ]
