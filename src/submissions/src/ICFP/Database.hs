{-# LANGUAGE OverloadedStrings #-}

module ICFP.Database where

import           Control.Lens
import           Control.Applicative ((<$>))
import           Crypto.Random
import qualified Data.ByteString.Base64 as B64
import           Data.Maybe (catMaybes, fromJust, fromMaybe, listToMaybe)
import           Data.Monoid ((<>))
import           Data.String (fromString)
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8)
import           Data.Time.Clock (getCurrentTime)
import           Database.PostgreSQL.Simple

import ICFP.User
import ICFP.Solution
import ICFP.Team

createUser :: Connection -> User -> IO (Maybe User)
createUser conn user =
  let q = "insert into users (email, created_at, api_token) values (lower(?),?,?) " <>
          "returning id, email, created_at, api_token"
  in listToMaybe <$> query conn q user

findUserByToken :: Connection -> ApiToken -> IO (Maybe User)
findUserByToken conn token =
  let q = "select id, email, created_at, api_token from users where api_token = ? limit 1"
  in listToMaybe <$> query conn q (Only token)

findUserByEmail :: Connection -> Email -> IO (Maybe User)
findUserByEmail conn addr =
  let q = "select id, email, created_at, api_token from users where lower(email) = lower(?) limit 1"
  in listToMaybe <$> query conn q (Only addr)

findOrCreateUserByEmail :: Connection -> Email -> IO User
findOrCreateUserByEmail conn email = do
  u <- findUserByEmail conn email
  case u of
    Just user -> return user
    Nothing   -> do
      now   <- getCurrentTime
      token <- either (error "Error generating API token") id <$> mkApiToken
      let user = User { _userId        = Nothing
                      , _userEmail     = email
                      , _userCreatedAt = now
                      , _userApiToken  = token
                      }
      user' <- fromJust <$> createUser conn user  -- force error on failure
      return user'

createTeam :: Connection -> Team -> IO Int
createTeam conn team = do
  let q = "insert into teams (name, created_at) values (?, ?) returning id"
  now <- Just <$> getCurrentTime
  withTransaction conn $ do
    (Only theTeamId):_ <- query conn q (team & teamCreatedAt .~ now)
    membersRes <- addMembers conn theTeamId (team^.teamMembers)
    case membersRes of
      Just _  -> return theTeamId
      Nothing -> error "Error creating team"

updateTeam :: Connection -> Int -> Team -> IO ()
updateTeam conn theTeamId team = do
  let q = "update teams set name = ? where id = ?"
  withTransaction conn $ do
    _ <- fromJust . expected (== 1) <$> execute conn q (team^.teamName, theTeamId)
    _ <- fromJust <$> addMembers conn theTeamId (team^.teamMembers)
    return ()

addMembers :: Connection -> Int -> [Email] -> IO (Maybe ())
addMembers conn theTeamId emails = do
  results <- mapM (addMember conn theTeamId) emails
  -- this will tell us if at least one insert succeeded
  return $ listToMaybe (catMaybes results)

addMember :: Connection -> Int -> Email -> IO (Maybe ())
addMember conn theTeamId email = do
  user <- findOrCreateUserByEmail conn email
  let q = "insert into teams_users (team_id, user_id) select ?, ? " <>
          "where not exists (select 1 from teams_users where team_id = ? and user_id = ?)"
  expected (== 1) <$> execute conn q (theTeamId, user^.userId, theTeamId, user^.userId)

findTeamsByUser :: Connection -> User -> IO [Team]
findTeamsByUser conn user = do
  let q = "select teams.id, teams.name, teams.created_at from teams " <>
          "inner join teams_users on team_id = teams.id " <>
          "where user_id = ?"
  teams <- query conn q (Only (user^.userId))
  mapM (populateUsers conn) teams

populateUsers :: Connection -> Team -> IO Team
populateUsers conn team = do
  -- We don't want to expose the apiToken here, so we get 'users.email' instead.
  let q = "select users.id, users.email, users.created_at, users.email from users " <>
          "inner join teams_users on user_id = users.id " <>
          "where team_id = ?"
  users <- query conn q (Only (team^.teamId))
  return $ team & teamMembers .~ (users ^.. traverse . userEmail)

isTeamMember :: Connection -> Int -> Int -> IO Bool
isTeamMember conn theTeamId theUserId = do
  let q = "select 1 from teams_users where team_id = ? and user_id = ?"
  not . null <$> (query conn q (theTeamId, theUserId) :: IO [Only Int])

createSolutions :: Connection -> Int -> User -> [UploadedSolution] -> IO (Maybe ())
createSolutions conn theTeamId user ss = do
  let q = "insert into solutions (team_id, author_id, created_at, problem_id, seed, score, power_score, content_path, tag) values (?, ?, ?, ?, ?, ?, ?, ?, ?)"
  now <- getCurrentTime
  expected (>= 0) <$> executeMany conn q (prep now <$> ss)
  where
    prep t s = Solution
      { _solutionTeamId    = theTeamId
      , _solutionAuthorId  = fromJust (user ^. userId)
      , _solutionCreatedAt = t
      , _solutionProblemId = s ^. uploadedProblemId
      , _solutionSeed      = s ^. uploadedSeed
      , _solutionScore     = Nothing
      , _solutionPowerScore = Nothing
      , _solutionSolution  = s ^. uploadedSolution
      , _solutionTag       = fromMaybe (fromString (show t)) $ s ^. uploadedTag
      }

findSolutionsByTeamId :: Connection -> Int -> IO [Solution]
findSolutionsByTeamId conn theTeamId =
  let q = "select team_id, author_id, created_at, problem_id, seed, score, power_score, content_path, tag from solutions where team_id = ?"
  in query conn q (Only theTeamId)

expected :: (Eq a, Integral a) => (a -> Bool) -> a -> Maybe ()
expected p affectedRows = if p affectedRows then Just () else Nothing

mkApiToken :: IO (Either GenError Text)
mkApiToken = do
  g <- newGenIO :: IO SystemRandom
  return $ (decodeUtf8 . B64.encode . fst) <$> genBytes 32 g
