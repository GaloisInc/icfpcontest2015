{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main(main) where

import Hex
import Problem
import DB
import Export
import Power

import           Data.Word
import qualified Data.Aeson as JS
import           System.Exit(exitFailure)
import           System.IO(hPutStrLn,stderr)
import           System.Environment(getArgs)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map as Map
import           Data.Char(toLower)
import           Data.List(isPrefixOf,sortBy)
import           Text.Read(readMaybe)
import           Data.Maybe(fromMaybe)
import           Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Control.Monad(forM_,unless,when)


main :: IO ()
main =
  do args <- getArgs
     case args of
       [ "check-stdin", powerWordFile, teamIdTxt, conn_str ]
          | Just teamId <- readMaybe teamIdTxt ->
          do pws <- getPowerWords powerWordFile
             bs <- LBS.getContents
             case JS.decode bs of
               Nothing   ->
                 do hPutStrLn stderr "Failed to decode solutions"
                    putStrLn "[]"
               Just slns ->
                 do tn <- withDB conn_str $ \c -> getTeamName c teamId
                    print =<< scoreSolutions pws teamId tn slns

       [ "check", powerWordFile, pidTxt, seedTxt ]
        | Just pid <- readMaybe pidTxt, Just seed <- readMaybe seedTxt ->
            do allPws <- getPowerWords powerWordFile
               mbP <- loadProblem pid
               p <- case mbP of
                      Nothing -> reportError "No such problem"
                      Just p -> return p
               cs <- Text.getContents
               let (moves, mbErr, pts, pws) = evaluateOne allPws p seed cs
                   errMsg = case mbErr of
                              Nothing -> "no error"
                              Just err ->
                                 case err of
                                   InvalidCommand -> "invalid command: " ++ [cs `Text.index` fromIntegral moves]
                                   CommandCausedError c _ -> "command caused error: " ++ show c
                                   CommandAfterEnd -> "command after end"

               putStrLn $ unlines [ "moves: " ++ show moves
                                  , "error: " ++ errMsg
                                  , "score: " ++ show pts
                                  , "power: " ++ show pws
                                  ]

       [ "delete", n, conn_str ] | Just pid <- readMaybe n ->
          withDB conn_str $ \c -> print =<< deleteInvalidSolutions c pid

       [ limitTxt, conn_str ]
          | Just limit <- readMaybe limitTxt -> withDB conn_str $ \c ->
          let loop n =
                do todo <- getUnscored c
                   forM_ todo $ \(slnId, prob, seed, sln) ->
                     do mbP <- loadProblem prob
                        case mbP of
                          Nothing -> setScore c slnId 0 []
                          Just p ->
                            do let (_,_,points,usedPower) = evaluateOne defaultPowerWords p seed sln
                               setScore c slnId points usedPower

                   unless (null todo || n > limit) (loop (n+1))
                   when (n > limit)
                     (putStrLn "WARNING: Couldn't finish everything!")
          in loop (0 :: Integer)

       _ -> reportError "Parameters: LIMIT DB_CONNECTION_STRING"







evaluateOne :: [Text] -> Problem -> Int -> Text ->
                                (Integer, Maybe GameError, Integer, [Int])
evaluateOne pws p seed sln
  | not (fromIntegral seed `elem` problemSeeds p) = (0, Just InvalidCommand, 0, [])  -- don't score things that are not in the problem
  | otherwise =
  let cs  = decodeText sln
      g   = playGame (fromIntegral seed) p cs
      (powerScore,usedPower') = scorePower pws sln
  in case gameState g of
       GameOver (GameError err) -> (gameMoves g, Just err,0,[])
       _ -> (gameMoves g, Nothing,gameScore g + powerScore, usedPower')


getProblem :: Int -> IO Problem
getProblem n =
  do mbP <- loadProblem n
     case mbP of
       Nothing -> fail "Failed to load problem"
       Just p  -> return p

scoreSolutions :: [Text] -> Integer -> Text -> [Solution] -> IO [Sln]
scoreSolutions pws slnTeamId slnTeamName slns =
  do entries <- mapM (scoreSolution pws) slns
     let cmp (p1,_,_,_) (p2,_,_,_) = compare p1 p2
     return
        [ Sln { slnTag = [], .. }
             | (slnProblem,slnSeed,slnScore,slnPower) <- sortBy cmp entries ]



scoreSolution :: [Text] -> Solution -> IO (Int,Word32,Integer,[Int])
scoreSolution pws Solution { .. } =
  do p <- getProblem solutionProblemId
     let (_,_,s,xs) = evaluateOne pws p solutionProblemSeed solutionCmds
     return (solutionProblemId,fromIntegral solutionProblemSeed,s,xs)





_oldMain :: IO ()
_oldMain =
  do args <- getArgs
     case args of
       [f,s] | Just seed <- readMaybe s ->
         do bs <- LBS.readFile f
            case JS.decode bs of
              Just p ->
                do cs <- getCommands
                   let g = playGame seed p cs

                   putStrLn ("moves " ++ show (gameMoves g) ++
                             " score " ++ show (gameScore g))

              Nothing ->
                reportError ("Malformed problem in file: " ++ show f)

       _   -> reportError "Parameters: FILE SEED LENGTH"


reportError :: String -> IO a
reportError e = do hPutStrLn stderr e
                   exitFailure




getCommands :: IO [Maybe Command]
getCommands =
  do txt <- getContents
     return (map decodeChar (filter (not . ignore) txt))






