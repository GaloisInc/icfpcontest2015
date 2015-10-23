{-# LANGUAGE OverloadedStrings #-}
module Export (Export(..), tag, (.=), JS.object
              , JS.FromJSON(..), (JS..:), JS.Value(..) ) where

import qualified Data.Aeson as JS
import qualified Data.Aeson.Types as JS
import           Data.Text(Text)
import           Data.Vector(Vector)
import qualified Data.Vector as Vector
import           Data.Word(Word32)

tag :: Text -> JS.Pair
tag x = "tag" JS..= x

(.=) :: Export a => Text -> a -> JS.Pair
x .= y = x JS..= toJSON y

class Export a where
  toJSON :: a -> JS.Value

instance Export Int where
  toJSON x = JS.toJSON x

instance Export Integer where
  toJSON x = JS.toJSON (show x)

instance Export Word32 where
  toJSON x = JS.toJSON x

instance Export Bool where
  toJSON x = JS.toJSON x

instance Export Text where
  toJSON x = JS.toJSON x

instance Export a => Export (Vector a) where
  toJSON xs = JS.Array (fmap toJSON xs)

instance Export a => Export [a] where
  toJSON xs = JS.Array (Vector.fromList (map toJSON xs))

instance Export a => Export (Maybe a) where
  toJSON = maybe JS.Null toJSON

