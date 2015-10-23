{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           Codec.MIME.String.ContentDisposition
import           Control.Applicative
import           Control.Lens
import           Control.Monad.Error.Class (MonadError, throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import           Data.Maybe (fromJust, fromMaybe)
import           Data.Monoid ((<>))
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Time.Clock (getCurrentTime)
import           Database.PostgreSQL.Simple (connectPostgreSQL)
import           Network.Wai (Application)
import           Network.Wai.Handler.Warp (run)
import           Servant
import           System.Directory (createDirectoryIfMissing)
import           System.Environment (getEnv)

import qualified Network.HTTP.Authorization as Auth
import           Network.HTTP.ContentDisposition (getFilename)
import           ICFP.Config
import           ICFP.Database hiding (createTeam, updateTeam)
import qualified ICFP.Database as DB
import           ICFP.Session
import           ICFP.Solution
import           ICFP.Team
import           ICFP.User

type IcfpSubmissions
  =    "session"                                    :> CreateSession

  :<|> "teams" :> ReqBody '[JSON] Team :> Protected :> Post '[PlainText] Text
  :<|> "teams" :> Protected :> Get  '[JSON] [Team]

  :<|> "teams" :> Capture "teamId" Int :> ReqBody '[JSON] Team
               :> Protected :> Put '[PlainText] Text

  :<|> "teams" :> Capture "teamId" Int :> "solutions"
               :> ReqBody '[JSON] [UploadedSolution]
               :> Protected :> Post '[PlainText] Text
  :<|> "teams" :> Capture "teamId" Int :> "solutions"
               :> Protected :> Get '[JSON] [Solution]

  :<|> "teams" :> Capture "teamId" Int :> "implementation"
               :> ReqBody '[OctetStream] ByteString
               :> Header "Content-Disposition" ContentDisposition
               :> Protected :> Post '[PlainText] Text

  :<|> "js"    :> Raw
  :<|> Raw

type Protected = Header "Authorization" Auth.Basic
type Servant a = forall m. (Applicative m, MonadIO m, MonadError ServantErr m) => m a

server :: Config -> Server IcfpSubmissions
server config =
         createSession config
    :<|> createTeam
    :<|> getTeams
    :<|> updateTeam
    :<|> postSolutions
    :<|> fetchSolutions
    :<|> postImplementation
    :<|> serveDirectory "client/dist"
    :<|> serveDirectory "static"
  where
    createTeam :: Team -> Maybe Auth.Basic -> Servant Text
    createTeam team auth = protected auth $ \user -> do
      let withCreator = team & teamMembers %~ ((user^.userEmail):)
      _ <- liftIO $ DB.createTeam (config^.connection) withCreator
      return "created"
      -- TODO: a 'Location' header in the response would be nice

    getTeams :: Maybe Auth.Basic -> Servant [Team]
    getTeams auth = protected auth $ \user -> do
      liftIO $ findTeamsByUser (config^.connection) user

    updateTeam :: Int -> Team -> Maybe Auth.Basic -> Servant Text
    updateTeam theTeamId team auth = protected auth $ withTeam theTeamId $ \_ -> do
      liftIO $ DB.updateTeam (config^.connection) theTeamId team
      return "updated"

    postSolutions :: Int -> [UploadedSolution] -> Maybe Auth.Basic -> Servant Text
    postSolutions theTeamId solutions auth = protected auth $ withTeam theTeamId $ \user -> do
      res <- liftIO $ createSolutions (config^.connection) theTeamId user solutions
      case res of
        Just _  -> return "created"
        Nothing -> throwError err400

    fetchSolutions :: Int -> Maybe Auth.Basic -> Servant [Solution]
    fetchSolutions theTeamId auth = protected auth $ withTeam theTeamId $ \_ ->
      liftIO $ findSolutionsByTeamId (config^.connection) theTeamId

    postImplementation :: Int -> ByteString
                       -> Maybe ContentDisposition -> Maybe Auth.Basic -> Servant Text
    postImplementation theTeamId impl cd auth = protected auth $ withTeam theTeamId $ \_ -> do
      now <- liftIO $ getCurrentTime
      let filename = fromMaybe "impl" (cd >>= getFilename)
      let destdir  = (config^.filestore)
                   <> "/implementations" <> "/" <> show theTeamId <> "/" <> show now
      let destpath = destdir <> "/" <> Text.unpack filename
      liftIO $ createDirectoryIfMissing True destdir
      liftIO $ LBS.writeFile destpath impl
      return "created"

    protected :: Maybe Auth.Basic -> (User -> Servant a) -> Servant a
    protected Nothing _ = throwError err401
    protected (Just auth) f = do
      let token = Auth.basicPass auth
      u <- liftIO $ findUserByToken (config^.connection) token
      case u of
        Nothing   -> throwError err401 { errBody = "authentication problem" }
        Just user -> f user

    withTeam :: Int -> (User -> Servant a) -> User -> Servant a
    withTeam theTeamId f user = do
      member <- liftIO $ isTeamMember (config^.connection) theTeamId (fromJust (user^.userId))
      if member
      then f user
      else throwError err401 { errBody = "you are not a member of that team" }


icfpSubmissions :: Proxy IcfpSubmissions
icfpSubmissions = Proxy

app :: Config -> Application
app config = serve icfpSubmissions (server config)

main :: IO ()
main = do
  dbUri <- fromString <$> getEnv "DBM_DATABASE"
  aud   <- fromString <$> getEnv "AUDIENCE"  -- e.g.: "https://davar.icfp2015.org"
  port  <- read <$> getEnv "PORT"
  fstore <- fromString <$> getEnv "FILE_STORE"
  conn  <- connectPostgreSQL dbUri
  let config = Config { _connection = conn, _audience = aud, _filestore = fstore }
  Text.putStrLn ("Running app on " <> aud <> "...")
  run port (app config)
