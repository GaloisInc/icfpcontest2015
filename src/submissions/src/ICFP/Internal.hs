module ICFP.Internal where

import Data.Char

stripRecordPrefix :: String -> String
stripRecordPrefix l =
  let (x:xs) = dropWhile (not . isUpper) l
  in toLower x : xs
