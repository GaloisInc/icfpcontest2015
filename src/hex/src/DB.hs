{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}
module DB where

import Problem

import Data.Int
import Data.Word(Word32)
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Data.Bits
import Data.String(fromString)

import System.Directory(createDirectoryIfMissing)
import System.FilePath((</>))
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as Set
import Data.List(groupBy)
import Data.Monoid


withDB :: String -> (Connection -> IO a) -> IO a
withDB s f =
  do c <- connectPostgreSQL (BS.pack s)
     r <- f c
     close c
     return r

-- | Get submitted solutions that have no score.
-- Limit to a certain number, to avoid using up all memory
getUnscored :: Connection -> IO [(Integer,Int,Int,Text)]
getUnscored c =
  query_ c [sql| SELECT id, problem_id, seed, content_path
                 FROM solutions
                 WHERE score IS NULL
                 LIMIT 1000 |]

-- | Set the score for a specific solution id.
setScore :: Connection -> Integer -> Integer -> [Int] -> IO Int64
setScore c solutionId score powerWords =
  do execute c
            [sql| UPDATE solutions SET score = ?, power_score = ?  WHERE id = ?  |]
            (score, powerWordsToNum powerWords, solutionId)


deleteInvalidSolutions :: Connection -> Int -> IO Int64
deleteInvalidSolutions c n =
  do mb <- loadProblem n
     case mb of
       Nothing -> return 0
       Just p ->
         do let cts = [ "seed != " <> fromString (show s) | s <- problemSeeds p ]
            do execute_ c ("DELETE FROM solutions WHERE problem_id = " <>
                        fromString (show (problemId p)) <>
                        " AND " <> foldr1 (\x y -> x <> " AND " <> y) cts)


getTeamName :: Connection -> Integer -> IO Text
getTeamName c n =
  do x <- query c [sql| SELECT name FROM teams WHERE id = ? LIMIT 1 |] (Only n)
     case map fromOnly x of
       [y] -> return y
       _   -> fail ("Failed to get team name for: " ++ show n)

-- | List all teams in the database.
getTeams :: Connection -> IO [(Integer,Text)]
getTeams c = query_ c [sql| SELECT id,name FROM teams |]

-- | Given a team, solution, seed, get the latest score, if any
getScore :: Connection ->
              Integer -> Int -> Word32 -> IO (Integer, [Int], [Text])
getScore c teamId problemId seed =
  do xs <- query c
        [sql| SELECT score,power_score,tag
              FROM solutions
              WHERE team_id    = ? AND
                    problem_id = ? AND
                    seed       = ? AND
                    (score IS NOT NULL)
              ORDER BY created_at DESC LIMIT 1 |]
              (teamId, problemId, seed)
     return $ case xs of
                []          -> (0, [], [])
                (x,y,z) : _ -> (x, powerWordsFromNum y, [z])


powerWordsToNum :: [Int] -> Integer
powerWordsToNum = foldr (.|.) 0 . map toUnary
  where toUnary x = 1 `shiftL` x

powerWordsFromNum :: Integer -> [Int]
powerWordsFromNum x = [ n | n <- [ 0 .. 31 ], testBit x n ]




data Sln = Sln { slnTeamId     :: Integer
               , slnTeamName   :: Text
               , slnProblem    :: Int
               , slnSeed       :: Word32
               , slnTag        :: [Text]
               , slnScore      :: Integer
               , slnPower      :: [Int]
               } deriving (Read,Show)


{-
-- XXX: If we add sees to the database, then we don't need the list of problems
getLatestSolutions :: Connection -> [(Integer,Text)] -> [Problem] -> IO [Sln]
getLatestSolutions c ts ps =
  do as <- forM ts $ \(slnTeamId,slnTeamName) ->
             forM ps $ \Problem { .. } ->
               forM problemSeeds $ \slnSeed ->
                  do let slnProblem = problemId
                     (slnScore,slnPower,slnTag) <-
                              getScore c slnTeamId slnProblem slnSeed
                     return Sln { .. }
     return (concat (concat as))
-}


downloadLatestSolutions :: Connection -> [Problem] -> IO ()
downloadLatestSolutions c ps =
  do res <- query_ c [sql|
              select solutions.team_id,
                     solutions.problem_id,
                     solutions.seed,
                     score,
                     power_score,
                     tag,
                     teams.name,
                     content_path

              from ( select solutions.team_id,
                            solutions.problem_id,
                            solutions.seed,
                            max(id) as one

                     from (select team_id, problem_id, seed, max(created_at) as d
                           from solutions
                           where score is not null and power_score is not null
                           group by team_id, problem_id, seed) as M

                     join solutions on solutions.team_id = M.team_id and
                                       solutions.problem_id = M.problem_id and
                                       solutions.seed = M.seed and
                                       created_at = d
                     group by solutions.team_id, solutions.problem_id, solutions.seed
                   ) as N
              join solutions on id = one
              join teams on teams.id = solutions.team_id

              order by problem_id, team_id |]
     let ok = Set.fromList (map problemId ps)
         ans = [ (Sln { slnTeamId = team_id
                  , slnTeamName = name
                  , slnProblem = problem_id
                  , slnSeed    = fromIntegral (seed :: Integer)
                  , slnTag     = [tag]
                  , slnScore   = score
                  , slnPower   = powerWordsFromNum power_score
                  },content)
                  | (team_id, problem_id, seed, score, power_score, tag, name
                    , content)
                          <- res, problem_id `Set.member` ok ]
     mapM_ saveAns ans



  where
  prep sln =
    do let teamDir  = "teams" </> show (slnTeamId sln)
       createDirectoryIfMissing True teamDir
       Text.writeFile (teamDir </> "team_name") (slnTeamName sln)
       let prob_dir = teamDir </> show (slnProblem sln)
       createDirectoryIfMissing True prob_dir
       return prob_dir

  saveAns (sln,content) =
    do d <- prep sln
       let file = d </> show (slnSeed sln)
       Text.writeFile file content





getBestSolutions :: Connection -> [(Integer,Text)] -> [Problem] ->
                    IO [ ( Problem
                         , [ [Sln] ]   -- (solution, grouped by team)
                         )
                       ]
                      -- by problem, by team
getBestSolutions c ts ps =
  do res <- query_ c [sql|

       select distinct
          solutions.team_id,
          solutions.problem_id,
          solutions.seed,
          solutions.score,
          power_score,
          tag,
          teams.name
          from
            (select solutions.team_id, solutions.problem_id, solutions.seed, solutions.score, MAX(id) as prob_id
              from ( select distinct team_id, problem_id, seed, MAX(score) as score
                     from solutions
                     group by team_id, problem_id, seed
                   ) as best
              join solutions on best.team_id = solutions.team_id and
                                best.problem_id = solutions.problem_id and
                                best.seed = solutions.seed and
                                best.score = solutions.score

               group by solutions.team_id, solutions.problem_id, solutions.seed, solutions.score) as A

  join solutions on A.prob_id = solutions.id
  join teams on solutions.team_id = teams.id
  order by problem_id, team_id |]

     let ok = Set.fromList (map problemId ps)
         ans = [ Sln { slnTeamId = team_id
                  , slnTeamName = name
                  , slnProblem = problem_id
                  , slnSeed    = fromIntegral (seed :: Integer)
                  , slnTag     = [tag]
                  , slnScore   = score
                  , slnPower   = powerWordsFromNum power_score
                  }
                  | (team_id, problem_id, seed, score, power_score, tag, name)
                          <- res, problem_id `Set.member` ok ]

         grouped = [ (pr, groupBy sameTeam p)
                         | p <- groupBy sameProblem ans
                         , pr <- [ q | q <- ps
                                     , problemId q == slnProblem (head p) ]
                   ]

         sameProblem x y = slnProblem x == slnProblem y
         sameTeam x y    = slnTeamId x == slnTeamId y

     return grouped







getLatestSolutions' :: Connection -> [(Integer,Text)] -> [Problem] ->
                    IO [ ( Problem
                         , [ [(Sln,Text)] ]   -- (solution, grouped by team)
                         )
                       ]
                      -- by problem, by team
getLatestSolutions' c ts ps =
  do res <- query_ c [sql|
              select solutions.team_id,
                     solutions.problem_id,
                     solutions.seed,
                     score,
                     power_score,
                     tag,
                     teams.name,
                     solutions.content_path

              from ( select solutions.team_id,
                            solutions.problem_id,
                            solutions.seed,
                            max(id) as one

                     from (select team_id, problem_id, seed, max(created_at) as d
                           from solutions
                           where score is not null and power_score is not null
                           group by team_id, problem_id, seed) as M

                     join solutions on solutions.team_id = M.team_id and
                                       solutions.problem_id = M.problem_id and
                                       solutions.seed = M.seed and
                                       created_at = d
                     group by solutions.team_id, solutions.problem_id, solutions.seed
                   ) as N
              join solutions on id = one
              join teams on teams.id = solutions.team_id

              order by problem_id, team_id |]
     let ok = Set.fromList (map problemId ps)
         ans = [ (Sln { slnTeamId = team_id
                  , slnTeamName = name
                  , slnProblem = problem_id
                  , slnSeed    = fromIntegral (seed :: Integer)
                  , slnTag     = [tag]
                  , slnScore   = score
                  , slnPower   = powerWordsFromNum power_score
                  }, sln)
                  | ( team_id, problem_id, seed, score, power_score, tag
                    , name, sln )
                          <- res, problem_id `Set.member` ok ]

         grouped = [ (pr, groupBy sameTeam p)
                         | p <- groupBy sameProblem ans
                         , pr <- [ q | q <- ps
                                     , problemId q == slnProblem (fst (head p)) ]
                   ]

         sameProblem (x,_) (y,_) = slnProblem x == slnProblem y
         sameTeam (x,_) (y,_)    = slnTeamId x == slnTeamId y

     return grouped






getLatestSolutionsLight :: Connection -> [(Integer,Text)] -> [Problem] ->
                    IO [ ( Problem
                         , [ [(Sln,Text)] ]   -- (solution, grouped by team)
                         )
                       ]
                      -- by problem, by team
getLatestSolutionsLight c ts ps =
  do res <- query_ c [sql|
              select solutions.team_id,
                     solutions.problem_id,
                     solutions.seed,
                     score,
                     power_score,
                     tag,
                     teams.name,
                     solutions.content_path

              from ( select solutions.team_id,
                            solutions.problem_id,
                            solutions.seed,
                            max(id) as one

                     from (select team_id, problem_id, seed, max(created_at) as d
                           from solutions
                           where score is not null and power_score is not null
                             and created_at <= timestamp '2015-08-08 12:00:00' at time zone 'UTC'
                           group by team_id, problem_id, seed) as M

                     join solutions on solutions.team_id = M.team_id and
                                       solutions.problem_id = M.problem_id and
                                       solutions.seed = M.seed and
                                       created_at = d
                     group by solutions.team_id, solutions.problem_id, solutions.seed
                   ) as N
              join solutions on id = one
              join teams on teams.id = solutions.team_id

              order by problem_id, team_id
        |]


     let ok = Set.fromList (map problemId ps)
         ans = [ (Sln { slnTeamId = team_id
                  , slnTeamName = name
                  , slnProblem = problem_id
                  , slnSeed    = fromIntegral (seed :: Integer)
                  , slnTag     = [tag]
                  , slnScore   = score
                  , slnPower   = powerWordsFromNum power_score
                  }, sln)
                  | ( team_id, problem_id, seed, score, power_score, tag
                    , name, sln )
                          <- res, problem_id `Set.member` ok ]

         grouped = [ (pr, groupBy sameTeam p)
                         | p <- groupBy sameProblem ans
                         , pr <- [ q | q <- ps
                                     , problemId q == slnProblem (fst (head p)) ]
                   ]

         sameProblem (x,_) (y,_) = slnProblem x == slnProblem y
         sameTeam (x,_) (y,_)    = slnTeamId x == slnTeamId y

     return grouped






getLatestSolutions :: Connection -> [(Integer,Text)] -> [Problem] ->
                    IO [ ( Problem
                         , [ [Sln] ]   -- (solution, grouped by team)
                         )
                       ]
                      -- by problem, by team
getLatestSolutions c ts ps =
  do res <- query_ c [sql|
              select solutions.team_id,
                     solutions.problem_id,
                     solutions.seed,
                     score,
                     power_score,
                     tag,
                     teams.name

              from ( select solutions.team_id,
                            solutions.problem_id,
                            solutions.seed,
                            max(id) as one

                     from (select team_id, problem_id, seed, max(created_at) as d
                           from solutions
                           where score is not null and power_score is not null
                           group by team_id, problem_id, seed) as M

                     join solutions on solutions.team_id = M.team_id and
                                       solutions.problem_id = M.problem_id and
                                       solutions.seed = M.seed and
                                       created_at = d
                     group by solutions.team_id, solutions.problem_id, solutions.seed
                   ) as N
              join solutions on id = one
              join teams on teams.id = solutions.team_id

              order by problem_id, team_id |]
     let ok = Set.fromList (map problemId ps)
         ans = [ Sln { slnTeamId = team_id
                  , slnTeamName = name
                  , slnProblem = problem_id
                  , slnSeed    = fromIntegral (seed :: Integer)
                  , slnTag     = [tag]
                  , slnScore   = score
                  , slnPower   = powerWordsFromNum power_score
                  }
                  | (team_id, problem_id, seed, score, power_score, tag, name)
                          <- res, problem_id `Set.member` ok ]

         grouped = [ (pr, groupBy sameTeam p)
                         | p <- groupBy sameProblem ans
                         , pr <- [ q | q <- ps
                                     , problemId q == slnProblem (head p) ]
                   ]

         sameProblem x y = slnProblem x == slnProblem y
         sameTeam x y    = slnTeamId x == slnTeamId y

     return grouped







-- Testing --------------------------------------------------------------------
makeTeam :: Connection -> Text -> IO ()
makeTeam c name =
  do _ <- execute c
            [sql| INSERT INTO teams (name, created_at) VALUES (?, now()) |]
                                                                  (Only name)
     return ()

makeUser :: Connection -> Text -> Text -> IO ()
makeUser c name api =
  do _ <- execute c
            [sql| INSERT INTO users (created_at,email,api_token)
                  VALUES (now(),?,?) |]
                 (name,api)
     return ()



makeProblem :: Connection -> Text -> IO ()
makeProblem c name =
  do _ <- execute c
            [sql| INSERT INTO problems (content_path, created_at)
                  VALUES (?, now()) |] (Only name)
     return ()

makeSolution :: Connection -> Integer -> Integer -> Text -> Integer -> Integer -> Text -> IO ()
makeSolution c tid user tag pid seed d =
  do _ <- execute c
            [sql| INSERT INTO solutions ( created_at,problem_id,team_id
                                        , author_id, content_path, tag, seed
                                        )
                  VALUES (now(), ?, ?, ?, ?, ?, ?) |]
                 (pid,tid,user,d,tag,seed)
     return ()

_db :: (Connection -> IO a) -> IO a
_db = withDB "dbname=icfp_contest_2015"


