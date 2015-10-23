{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main(main) where

import Hex
import Export
-- import MakeProblem

import qualified Data.Aeson as JS
import           Data.IORef(newIORef, atomicModifyIORef)
import           Control.Monad.IO.Class(liftIO)
import qualified Snap.Http.Server as Snap
import qualified Snap.Core as Snap
import qualified Snap.Util.FileServe as Snap
import           System.FilePath((</>))
import           System.Environment(getArgs)
import           System.IO(hPutStrLn, stderr)
import           System.Exit(exitFailure)
import qualified Data.ByteString.Lazy as LBS
import           Text.Read(readMaybe)

main :: IO ()
main =
  do args <- getArgs
     (p,seed) <-
          case args of
            [ seedTxt, file ] | Just seed <- readMaybe seedTxt ->
              do bs <- LBS.readFile file
                 case JS.decode bs of
                   Just yes -> return (yes,seed)
                   Nothing  -> err "Failed to parse problem."

            _ -> err  "Parameters: SEED FILE"

     let start = newGameFromProblem seed p
     s <- newIORef start
     let act f =
           do g <- liftIO $ atomicModifyIORef s
                          $ \g -> let x = f g in (x,x)
              Snap.modifyResponse
                 (Snap.setHeader "content-type" "application/json")
              Snap.writeLBS (JS.encode (toJSON g))

     Snap.quickHttpServe $ Snap.route
       [ ("start",       act (\_ -> start))
       , ("get",         act id)
       , ("left",        act (gameCommand MoveLeft))
       , ("right",       act (gameCommand MoveRight))
       , ("down_left",   act (gameCommand MoveDownLeft))
       , ("down_right",  act (gameCommand MoveDownRight))
       , ("rot_clock",   act (gameCommand RotClock))
       , ("rot_anti",    act (gameCommand RotAnti))
       , ("rot_anti",    act (gameCommand RotAnti))
       , ("jquery.js",   Snap.serveFile ("web" </> "jquery.js"))
       , ("view.js",     Snap.serveFile ("web" </> "view.js"))
       , ("style.css",   Snap.serveFile ("web" </> "style.css"))
       , ("index.html",  Snap.serveFile ("web" </> "view.html"))
       ]
  where
  err msg = do hPutStrLn stderr msg
               exitFailure




