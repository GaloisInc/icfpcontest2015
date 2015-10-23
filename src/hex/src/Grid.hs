{-# LANGUAGE OverloadedStrings #-}
module Grid where

import Export

-- | Grid coordinates
data Grid = Grid !Int !Int deriving (Eq,Ord,Show,Read)

cmpRow :: Grid -> Grid -> Ordering
cmpRow (Grid _ y1) (Grid _ y2) = compare y1 y2

cmpCol :: Grid -> Grid -> Ordering
cmpCol (Grid x1 _) (Grid x2 _) = compare x1 x2

sameRow :: Grid -> Grid -> Bool
sameRow (Grid _ y1) (Grid _ y2) = y1 == y2

sameCol :: Grid -> Grid -> Bool
sameCol (Grid x1 _) (Grid x2 _) = x1 == x2

instance Export Grid where
  toJSON (Grid x y) = object [ "x" .= x, "y" .= y ]

instance FromJSON Grid where
  parseJSON v =
    case v of
      Object o -> do x <- o .: "x"
                     y <- o .: "y"
                     return (Grid x y)
      _ -> fail "Invalid `Grid`"
