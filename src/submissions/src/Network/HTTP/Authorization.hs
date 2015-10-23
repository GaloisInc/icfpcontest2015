{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.Authorization where

import qualified Data.ByteString.Base64 as B64
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Encoding (decodeUtf8', encodeUtf8)
import           GHC.Generics
import           Servant.Common.Text

data Basic = Basic { basicUser :: Text, basicPass :: Text  }
  deriving (Eq, Generic, Ord, Show)

data Challenge = Challenge { realm :: Text }

instance FromText Basic where
  fromText t = do
    encoded <- Text.stripPrefix "Basic " t
    decoded <- dropLeft (B64.decode (encodeUtf8 encoded))
    creds   <- dropLeft (decodeUtf8' decoded)
    let (user, pass) = Text.breakOn ":" creds
    return $ Basic user (Text.drop 1 pass)

instance ToText Challenge where
  toText (Challenge r) = "Basic realm=\"" <> r <> "\""

dropLeft :: Either a b -> Maybe b
dropLeft (Left  _) = Nothing
dropLeft (Right v) = Just v
