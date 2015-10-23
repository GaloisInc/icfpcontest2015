import System.Random(randomIO)
import qualified Data.Text.IO as Text
import Hex
import Text.Read(readMaybe)
import Control.Monad(replicateM_)
import System.Environment(getArgs)

main =
  do args <- getArgs
     case args of
       [n] -> case readMaybe n of
                Just x  -> replicateM_ x randCmd
                Nothing -> return ()
       _ -> return ()

randCmd =
  do x <- randomIO
     Text.putStr $ exportCommand $ if x then MoveDownLeft else MoveDownRight

