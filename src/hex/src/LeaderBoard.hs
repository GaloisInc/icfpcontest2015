{-# LANGUAGE OverloadedStrings, RecordWildCards, ParallelListComp #-}
module Main(main) where

import DB
import Problem
import Power
import Debug.Trace

import Data.Text(Text)
import qualified Data.Text as Text
import Data.List(sortBy,groupBy,union,nub,find)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Aeson as JS
import System.FilePath((</>))
import System.IO
import System.Time(getClockTime)
import System.Environment(getArgs)
import System.Exit(exitFailure)
import Control.Monad(forM)
import Text.Read(readMaybe)

main :: IO ()
main =
  do args <- getArgs
     case args of
       "combine" : ignoreTeamIdTxt : files
          | Just ignoreTeams <- readMaybe ignoreTeamIdTxt ->
          do gs <- forM files $ \f ->
                do bs0 <- LBS.readFile f
                   let bs = LBS.drop 11 bs0
                   case JS.decode bs of -- drop 'var data = '
                     Nothing -> fail $ unlines [ "Failed to parse: " ++ show f
                                               , LBS8.unpack $ LBS.take 30 bs ]
                     Just g  -> return g
             LBS.writeFile ("web" </> "rankings.js")
                  $ LBS.append ("var data = ")
                  $ JS.encode
                  $ combineProblems ignoreTeams gs




       how : name : probTxt : str : more 
        | Just probs <- readMaybe probTxt ->
         withDB str $ \c ->
            do t    <- getClockTime
               ts   <- getTeams c
               ps   <- getProblems
               sln' <- case how of
                         "latest" -> getLatestSolutions c ts ps
                         "best"   -> getBestSolutions c ts ps
                         "no_power"  ->
                            do res <- getLatestSolutions' c ts ps
                               return [ (p, map (map setLightScore) slns)
                                          | (p,slns) <- res ]
                         "light"  ->
                            do res <- getLatestSolutionsLight c ts ps
                               return [ (p, map (map setLightScore) slns)
                                          | (p,slns) <- res ]
                         "files" -> slnsFromFiles more ps
                         _ -> usage
               let useProblem p = problemId p `elem` (probs :: [Int])
               let slns = filter (useProblem . fst) sln'
               putStrLn ("Scoring: " ++ show (length slns) ++ " solutions.")


               let res = scoreProblems (Text.pack name) ts slns (filter useProblem ps)
               LBS.writeFile ("web" </> name ++ "-rankings.js")
                  $ LBS.append ("var data = ")
                  $ JS.encode res

       _ -> usage

  where
  usage = do hPutStrLn stderr "Parameters: [latest|best|no_power|light] DB_CONN"
             exitFailure



slnsFromFiles :: [FilePath] -> [Problem] -> IO [(Problem,[[Sln]])]
slnsFromFiles file problems =
  do ps <- forM file $ \file ->
              do txt <- readFile file
                 case readMaybe txt of
                   Nothing  -> fail ("Failed to parse: " ++ show file)
                   Just team_slns ->
                      return $ Map.fromListWith (++)
                                      [ (slnProblem p, [p]) | p <- team_slns ]
     let getP pid = case find ((== pid) . problemId) problems of
                      Just x -> x
                      Nothing -> error ("MISSSING PROBLEM: " ++ show pid)

     return [ (getP pid, slnss)
              | (pid,slnss) <- Map.toList $ Map.unionsWith (++)
                                 [ Map.map (:[]) m | m <- ps ] ]



setLightScore :: (Sln, Text) -> Sln
setLightScore (sln, txt) = sln { slnScore = max 0 (slnScore sln - p) }
  where (p,_) = scorePower defaultPowerWords txt

data LocalRanking = LocalRanking
  { locationName :: Int
  , rankings     :: [ Ranking ]    -- Sorted
  }

data Ranking = Ranking
  { rankNumber      :: Integer
  , rankTeamId      :: Integer
  , rankTeamName    :: Text
  , rankScore       :: Integer
  , rankPowerScore  :: [Int]
  , rankTags        :: [Text]
  }

data GlobResult = GlobResult
  { globRankings  :: [ Ranking ] -- power score is meaningless here
  , localRankings :: [ (Text, [ LocalRanking ]) ]
  }

scoreProblem :: [(Integer,Text)] -> (Problem, [ [Sln] ]) -> LocalRanking
scoreProblem teams (p,scores) =
  LocalRanking
    { locationName = problemId p
    , rankings     = makeRanks (sortBy cmpSln solutions)
    }

  where
  -- Bigger is better, hence swapped
  cmpSln s1 s2 = compare (slnScore s2) (slnScore s1)

  solutions = Map.elems ( Map.fromList
                [ (slnTeamId s
                  , s { slnScore = sum (map slnScore team) `div` variantNum
                      , slnPower = nub (concatMap slnPower team)
                      , slnTag   = nub (concatMap slnTag team)
                      }
                  )
                      | team <- scores, if null team then trace ("No solutions for problem: " ++ show (problemId p)) False else True, let s = head team ]

         `Map.union` Map.fromList [ (x,blankSln x y) | (x,y) <- teams ])


  blankSln x y = Sln { slnTeamId      = x
                     , slnTeamName    = y
                     , slnProblem     = problemId p
                     , slnSeed        = 0 -- doesn't matter
                     , slnTag         = []
                     , slnScore       = 0
                     , slnPower       = []
                     }

  variantNum = fromIntegral (length (problemSeeds p))

-- Add up scores and tags
jnSlns :: [Sln] -> Sln
jnSlns [] = error "no solutions"
jnSlns (x:xs) = x { slnScore      = round
                                 $ fromIntegral (sum (map slnScore xs)) /
                                   fromIntegral (length (x:xs))
                 , slnPower      = nub (concatMap slnPower (x:xs))
                 , slnTag        = nub (concatMap slnTag (x:xs))
                 }



-- | Make rankings out of a sorted list.
makeRanks :: [Sln] -> [Ranking]
makeRanks = concat . zipWith rank [ 1 .. ] . groupBy sameScore
  where
  sameScore x y = slnScore x == slnScore y
  rank n xs =
    [ Ranking { rankScore      = slnScore
              , rankPowerScore = slnPower
              , rankTeamId     = slnTeamId
              , rankTeamName   = slnTeamName
              , rankNumber     = n
              , rankTags       = slnTag
              } | Sln { .. } <- xs ]





scoreProblems :: Text -> [(Integer,Text)] -> [(Problem,[[Sln]])]
                                          -> [Problem] -> GlobResult
scoreProblems variant teams scores problems =
  GlobResult
    { globRankings  = makeRanks (sortBy theirRank overall)
    , localRankings = [ (variant, ranks) ]
    }
  where
  ranks = sortBy cmpProblemId $ map (scoreProblem teams) (blanks ++ scores)
  blanks = [ (p,[ [] | t <- teams ])
            | p <- problems, not (problemId p `elem` map (problemId . fst) scores) ]

  cmpProblemId x y = compare (locationName x) (locationName y)


  -- smaller is better
  theirRank s1 s2 = compare (slnScore s1) (slnScore s2)

  overall = map jnSlns
          $ Map.elems
          $ Map.fromListWith (++)
              [ (rankTeamId r, [rankToSln m r])
                        | LocalRanking { .. } <- ranks,
                          let m = maximum (map rankScore rankings),
                          r <- rankings ]


filterLocalHackPower :: Int -> [Integer] -> LocalRanking -> LocalRanking
filterLocalHackPower n ignoreThese LocalRanking { .. } =
  LocalRanking { rankings = [ r { rankPowerScore = map (+n) (rankPowerScore r) }
                            | r <- rankings,
                              not (rankTeamId r `elem` ignoreThese) ]
               , .. }

combineProblems :: [Integer] -> [GlobResult] -> GlobResult
combineProblems ignoreTeam gs =
  GlobResult
    { globRankings  = makeRanks (sortBy theirRank overall)
    , localRankings = ranks
    }
  where
  ranks = [ (v, map (filterLocalHackPower (powerOffset v) ignoreTeam) rs)
              | (v,rs) <- concatMap localRankings gs ]

  -- assumes not more than 100 power words per set
  allVariants = zip (nub (map fst ranks)) [ 0, 100 .. ]
  powerOffset v = case lookup v allVariants of
                    Just n  -> n
                    Nothing -> error ("Missing variant: " ++ show v)


  theirRank s1 s2 = compare (slnScore s2) (slnScore s1)

  overall = map jnSlns
          $ Map.elems
          $ Map.fromListWith (++)
              [ (rankTeamId r, [rankToSln m r])
                        | LocalRanking { .. } <- concatMap snd ranks
                        , let m = maximum (map rankScore rankings)
                        ,  r <- rankings ]

rankToSln :: Integer -> Ranking -> Sln
rankToSln maxScore Ranking { .. } =
    Sln { slnTeamId     = rankTeamId
        , slnTeamName   = rankTeamName
        , slnProblem    = 0
        , slnSeed       = 0
        , slnScore      =
            let s = fromIntegral rankScore :: Double
                m = (s / fromIntegral maxScore)^2
            in round (100 * m)

        , slnPower      = rankPowerScore
        , slnTag        = rankTags
        }



instance JS.FromJSON Ranking where
  parseJSON = JS.withObject "Invliad `Ranking`" $ \o ->
                do rankNumber     <- o .: "rank"
                   rankTeamId     <- o .: "teamId"
                   rankTeamName   <- o .: "team"
                   rankScore      <- o .: "score"
                   rankPowerScoreN  <- o .: "power_score"
                   let rankPowerScore = [ 1 .. rankPowerScoreN ] -- Doesn't matter
                   rankTags <- o .: "tags"
                   return Ranking { .. }

instance JS.FromJSON LocalRanking where
  parseJSON = JS.withObject "Invalid `LocalRanking`" $ \o ->
                do locationName <- o .: "setting"
                   rankings     <- o .: "rankings"
                   return LocalRanking { .. }


instance JS.FromJSON GlobResult where
  parseJSON = JS.withObject "Invalid `GlobResults`" $ \o ->
                do globRankings <- o .: "rankings"
                   ents <- o .: "variants"
                   localRankings <- mapM getV ents
                   return GlobResult { .. }
    where getV = withObject "Invalid 'Variant'" $ \o ->
                    do x <- o .: "name"
                       y <- o .: "settings"
                       return (x,y)



instance JS.ToJSON Ranking where
  toJSON Ranking { .. } =
    JS.object [ "rank"        .= rankNumber
              , "teamId"      .= rankTeamId
              , "team"        .= rankTeamName
              , "score"       .= rankScore
              , "power_score" .= length rankPowerScore
              , "tags"        .= rankTags
              ]

instance JS.ToJSON LocalRanking where
  toJSON LocalRanking { .. } =
    JS.object
      [ "setting"  .= locationName
      , "rankings" .= rankings
      ]

instance JS.ToJSON GlobResult where
  toJSON GlobResult { .. } =
    JS.object
      [ "rankings" .= globRankings
      , "variants" .= [ JS.object [ "name" .= x, "settings" .= y ]
                                                  | (x,y) <- localRankings ]
      ]



