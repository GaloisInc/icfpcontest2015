{-# LANGUAGE OverloadedStrings #-}
module Power where

import Hex
import Data.Text(Text)
import qualified Data.Text as Text
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Char
import Data.Maybe
import Data.List(isPrefixOf)
import Text.Read(readMaybe)

getPowerWords :: FilePath -> IO [Text]
getPowerWords file =
  do txt <- readFile file
     case readMaybe txt of
       Just xs -> return xs
       Nothing -> fail ("Failed to parse power file: " ++ show file)




scorePower :: [Text] -> Text -> (Integer,[Int])
scorePower pws cmds = (totalScore, mapMaybe findIndex (Map.keys results))
  where
  results     = countOccurances normPowerWords (Text.map toLower cmds)
  totalScore  = scorePowerMap results
  findIndex xs = lookup xs (zip normPowerWords [ 0 .. ])
  normPowerWords = map (Text.map toLower) pws


scorePowerMap :: Map Text Integer -> Integer
scorePowerMap results = Map.foldrWithKey score 0 results
  where
  score p n s
    | n > 0     = 300 + 2 * fromIntegral (Text.length p) * n + s
    | otherwise = s




findPower :: [Text] -> Text -> [(Integer,Text)]
findPower [] _ = []
findPower pws cs0 = go 0 (map toLower (Text.unpack cs0))
  where
  normPowerWords = map (Text.map toLower) pws

  checkOne n haystack needle db
    | Text.unpack needle `isPrefixOf` haystack = (n,needle) : db
    | otherwise                                = db

  checkAll n haystack db = foldr (checkOne n haystack) db normPowerWords

  go n cs
    | null cs = []
    | otherwise = let n' = n + 1
                  in seq n' (checkAll n cs (go n' (tail cs)))




countOccurances :: [Text] -> Text -> Map Text Integer
countOccurances needles = go Map.empty . Text.unpack
  where
  checkOne haystack needle db
    | Text.unpack needle `isPrefixOf` haystack = Map.insertWith (+) needle 1 db
    | otherwise                    = db

  checkAll haystack db = foldr (checkOne haystack) db needles

  go n haystack
    | null haystack = n
    | otherwise = let n' = checkAll haystack n
                  in n' `seq` go n' (tail haystack)


ignore :: Char -> Bool
ignore x = x == '\n' || x == '\r' || x == '\t'

decodeText :: Text -> [Maybe Command]
decodeText = map decodeChar . filter (not . ignore) . Text.unpack

commandReps :: Command  -> [Char]
commandReps cmd =
  case cmd of
    MoveLeft      -> "p\'!.03"
    MoveRight     -> "bcefy2"
    MoveDownLeft  -> "aghij4"
    MoveDownRight -> "lmno 5"
    RotClock      -> "dqrvz1"
    RotAnti       -> "kstuwx"

cmdMap :: Map Char Command
cmdMap = Map.fromList
  [ (c, cmd)
  | cmd <- [ MoveLeft, MoveRight, MoveDownLeft, MoveDownRight,
                                                      RotAnti, RotClock ]
  , c <- commandReps cmd
  ]

decodeChar :: Char -> Maybe Command
decodeChar c = Map.lookup (toLower c) cmdMap

_dump :: String -> IO ()
_dump = mapM_ (\x -> print . (,) x . fromJust . decodeChar $ x)

defaultPowerWords :: [Text]
defaultPowerWords =
  [ "Ei!"
  , "Ia! Ia!"
  , "R'lyeh"
  , "Yuggoth"
  , "Tsathoggua"
  , "YogSothoth"
  , "Necronomicon"
  , "vigintillion"
  , "Cthulhu fhtagn!"
  , "Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn."
  , "In his house at R'lyeh dead Cthulhu waits dreaming."
  , "The Laundry"
  , "Planet 10"
  , "Yoyodyne"
  , "monkeyboy"
  , "John Bigboote"
  , "BLUE HADES"
  , "CASE NIGHTMARE GREEN"
  ]





