{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.HTTP.ContentDisposition where

import           Codec.MIME.String.ContentDisposition
import           Data.List (find)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Servant.Common.Text

instance FromText ContentDisposition where
  fromText t = get_content_disposition (Text.unpack t)

getFilename :: ContentDisposition -> Maybe Text
getFilename (ContentDisposition _ ps) = find isFilename ps >>= fromFilename
  where
    isFilename (Filename s) = True
    isFilename _            = False
    
    fromFilename (Filename s) = Just (Text.pack s)
    fromFilename _            = Nothing
