{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Problem where

import Data.Word(Word32)
import Data.Maybe(fromMaybe)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as JS (encode,decode,(.:?))
import System.FilePath((</>), takeExtension)
import System.Directory(getDirectoryContents)
import qualified Data.Set as Set
import Control.Exception
import Data.Text(Text)

import Export
import Grid

data Solution = Solution
  { solutionProblemId   :: Int
  , solutionProblemSeed :: Int
  , solutionTag         :: Text
  , solutionCmds        :: Text
  }

instance FromJSON Solution where
  parseJSON v =
    case v of
      Object o -> do solutionProblemId   <- o .: "problemId"
                     solutionProblemSeed <- o .: "seed"
                     solutionTagMb       <- o JS..:? "tag"
                     let solutionTag = fromMaybe "" solutionTagMb
                     solutionCmds        <- o .: "solution"
                     return Solution { .. }

      _ -> fail "Invalid `Solution`"




-- | A problem description.
data Problem = Problem
  { problemId       :: !Int
  , problemShapes   :: [GridShape]
  , problemWidth    :: !Int
  , problemHeight   :: !Int
  , problemFilled   :: [Grid]
  , problemLen      :: !Int
  , problemSeeds    :: [Word32]
  } deriving (Read,Show)

problemDir :: FilePath
problemDir = "web" </> "problems"

problemFileById :: Int -> FilePath
problemFileById i = problemDir </> ("problem_" ++ show i ++ ".json")

problemFile :: Problem -> FilePath
problemFile = problemFileById . problemId


saveProblem :: Problem -> IO ()
saveProblem p = LBS.writeFile (problemFile p) (JS.encode (toJSON p))

loadProblemFromFile :: FilePath -> IO Problem
loadProblemFromFile file =
  do bs <- LBS.readFile file
     case JS.decode bs of
       Just p  -> return p
       Nothing -> fail ("Failed to parse problem from: " ++ show file)

loadProblem :: Int -> IO (Maybe Problem)
loadProblem i = fmap Just (loadProblemFromFile (problemFileById i))
                  `catch` \SomeException{} -> return Nothing

getProblems :: IO [Problem]
getProblems =
  do fds <- getDirectoryContents problemDir
     let fs = [ problemDir </> f | f <- fds, takeExtension f == ".json" ]
     mapM loadProblemFromFile fs


normalizeGridShape :: GridShape -> GridShape
normalizeGridShape GridShape { .. } =
  let mx = minimum [ x | Grid x _ <- members ]
      my = minimum [ y | Grid _ y <- members ]
      norm (Grid x y) = Grid (x - mx) (y - my)
  in GridShape { center = norm center, members = map norm members }



-- | A shape in grid coordinates.
data GridShape = GridShape { center :: Grid, members :: [Grid] {-distinct-} }
                    deriving (Show,Read)

instance Eq GridShape where
  x == y = center x == center y && Set.fromList (members x) == Set.fromList (members y)

-- | The width of a normalized (i.e. at (0,0)) shape.
gridShapeWidth :: GridShape -> Int
gridShapeWidth GridShape { .. } =
  maximum (0 : [ x + 1 | Grid x _ <- members ])

-- | The height of a normalized (i.e. at (0,0)) shape.
gridShapeHeight :: GridShape -> Int
gridShapeHeight GridShape { .. } =
  maximum (0 : [ y + 1 | Grid _ y <- members ])

-- | How many parts in this piece?
gridShapeSize :: GridShape -> Int
gridShapeSize GridShape { .. } = length members


instance Export GridShape where
  toJSON s = object [ "pivot" .= center s, "members" .= members s ]

instance FromJSON GridShape where
  parseJSON v =
    case v of
      Object o -> do center  <- o .: "pivot"
                     members <- o .: "members"
                     return GridShape { .. }
      _ -> fail "Invalid `GridShape`"

instance Export Problem where
  toJSON Problem { .. } =
    object
      [ "id"            .= problemId
      , "units"         .= problemShapes
      , "width"         .= problemWidth
      , "height"        .= problemHeight
      , "filled"        .= problemFilled
      , "sourceLength"  .= problemLen
      , "sourceSeeds"   .= problemSeeds
      ]

instance FromJSON Problem where
  parseJSON v =
    case v of
      Object o ->
        do problemId       <- o .: "id"
           problemWidth    <- o .: "width"
           problemHeight   <- o .: "height"
           problemFilled   <- o .: "filled"
           problemShapes   <- o .: "units"
           problemLen      <- o .: "sourceLength"
           problemSeeds    <- o .: "sourceSeeds"
           return Problem { .. }
      _ -> fail "Invalied `Problem`"


