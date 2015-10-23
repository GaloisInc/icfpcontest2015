{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Hex where

import Export
import Sequences
import Problem
import Grid

import Data.Word(Word32)
import           Data.Text(Text)
import Data.List(groupBy,sortBy,foldl')
import           Data.Vector (Vector)
import qualified Data.Vector as Vector

-- Geometry of the grid --------------------------------------------------------

data Dir = NE | E | SE | SW | W | NW
            deriving (Eq,Ord,Show,Enum,Bounded)

-- | Move a point in the given direction.
move :: Dir -> Grid -> Grid
move dir (Grid x y) =
  case dir of
    NE -> Grid (x+1 - ch) (y-1)
    E  -> Grid (x+1) y
    SE -> Grid (x+1 - ch) (y+1)
    SW -> Grid (x - ch)   (y+1)
    W  -> Grid (x-1) y
    NW -> Grid (x - ch) (y-1)
  where ch = if even y then 1 else 0

-- | Move a shape in a given direction.
moveGridShape :: Dir -> GridShape -> GridShape
moveGridShape dir GridShape { .. } =
  GridShape { center = move dir center, members = map (move dir) members }


-- Roatation -------------------------------------------------------------------

-- | Rotate a grid.
rotateGridShape :: Int -> GridShape -> GridShape
rotateGridShape x s = shapeToGrid (rotateShape x (gridToShape s)) (center s)

-- | Rotating directions.
rotate :: Int -> Dir -> Dir
rotate n = toEnum . (`mod` 6) . (n +) . fromEnum

-- | A shape description in polar coordinates.
newtype Shape = Shape [ Path ] deriving Show

-- | "Polar" coordinates
type Path     = [ Dir ]

-- | Rotate a shape in polar coordinates.
rotateShape :: Int -> Shape -> Shape
rotateShape x (Shape ps) = Shape (map (map (rotate x)) ps)

-- | Follow the path from the first point.
pathToGrid :: Path -> Grid -> Grid
pathToGrid p s = foldl' (flip move) s p

-- | Convert a shape in polar coordinates into grid coordinates,
-- with the center at the given location.
shapeToGrid :: Shape -> Grid -> GridShape
shapeToGrid (Shape sh) s = GridShape { center = s
                                     , members = map (`pathToGrid` s) sh
                                     }

-- | Compute a path from one grid location to another.
pathFromTo :: Grid -> Grid -> Path
pathFromTo p@(Grid x1 y1) q@(Grid x2 y2)
  | y1 < y2   = SE : pathFromTo (move SE p) q
  | y1 > y2   = NE : pathFromTo (move NE p) q
  | x1 < x2   = replicate (x2 - x1) E
  | x2 < x1   = replicate (x1 - x2) W
  | otherwise = []

-- | Convert a shape in grid coordinates into one using polar coorindates.
-- Note that this looses the position of the shape.
gridToShape :: GridShape -> Shape
gridToShape gs = Shape [ pathFromTo (center gs) m | m <- members gs ]

-- The board -------------------------------------------------------------------

-- | The configuariont of a board.
data Board = Board
  { boardWidth  :: Int
  , boardHeight :: Int
  , boardLayout :: Vector (Vector Bool)
  } deriving (Eq,Show)

-- | Make a new borard with the given dimensions, and the given initial setup.
newBoardWith :: Int -> Int -> [Grid] -> Board
newBoardWith boardWidth boardHeight gs =
  Board { boardLayout = Vector.fromList
                      $ padWith blank boardHeight 0
                      $ map row
                      $ groupBy sameRow
                      $ sortBy cmpRow gs
        , .. }

  where
  blank  = Vector.fromList (replicate boardWidth False)
  row xs = (y, Vector.fromList
                 $ padWith False boardWidth 0
                      [ (x,True) | Grid x _ <- sortBy cmpCol xs ])
    where Grid _ y = head xs

  padWith v n x [] = replicate (n - x) v
  padWith v n x ((a,b) : more)
    | x < a      = v : padWith v n (x + 1) ((a,b) : more)
    | otherwise  = b : padWith v n (x + 1) more


-- | Remove lines, and report how many we removed.
checkLines :: Board -> (Integer, Board)
checkLines b@Board { .. } = (fromIntegral lineNum, newB)
  where
  lineNum = Vector.foldl' (\c v -> if isLine v then c + 1 else c) 0 boardLayout
  isLine  = Vector.and

  newB
    | lineNum == 0 = b
    | otherwise = Board { boardLayout = Vector.replicate lineNum blank Vector.++
                                        Vector.filter (not . isLine) boardLayout
                        , .. }
  blank   = Vector.replicate boardWidth False

instance Export Board where
  toJSON Board { .. } = object [ "width"  .= boardWidth
                               , "height" .= boardHeight
                               , "layout" .= boardLayout
                               ]



-- The journey of one piece ----------------------------------------------------

-- | The current state of the process of a moving a single piece.
data Step = Step { shape :: GridShape
                 , board :: Board
                 , prevShapesRow :: [GridShape]
                   -- remembers the shape position before it was moved down;
                   -- this is to avoid repeate dboard positons.
                 } deriving Show

-- | Start a new step with the given shape and baord.
-- The piece starts centered (round towards smaller `x`),
-- with its top-most spot on the top line of the grid.
newStep :: GridShape -> Board -> Maybe Step
newStep nsh0 b = validate Step { shape = sh
                              , prevShapesRow = [sh]
                              , board = b }
  where sh        = iterate (moveGridShape E) nsh !! left
        left      = (w - sw) `div` 2
        w         = boardWidth b
        sw        = gridShapeWidth nsh
        nsh       = normalizeGridShape nsh0

-- | Is this a valid configuration for a step.
validate :: Step -> Maybe Step
validate g@Step { board = Board { .. }, shape = GridShape { .. } }
  | all ok members = Just g
  | otherwise      = Nothing
  where
  ok (Grid x y) =
   y >= 0 && y < boardHeight &&
   x >= 0 && x < boardWidth && not (boardLayout Vector.! y Vector.! x)

-- | Try to move a piece in the given direction.
moveThePiece :: Dir -> Step -> Maybe Step
moveThePiece d g = validate g { shape = moveGridShape d (shape g) }

-- | Try to rotate the piece, by that many rotations.  Positive is clockwise,
-- negative is anti-clockwise.
rotateThePiece :: Int -> Step -> Maybe Step
rotateThePiece n g = validate g { shape = rotateGridShape n (shape g) }

-- | Lock the piece onto the board, and return the new board.
lockThePiece :: Step -> Board
lockThePiece Step { board = Board { .. }, shape = GridShape { .. } } =
  Board { boardLayout = boardLayout Vector.// changes, .. }
  where
  changes = map updRow $ groupBy sameRow $ sortBy cmpRow members

  updRow gs = let Grid _ y : _ = gs
              in (y, (boardLayout Vector.! y)
                                  Vector.// [ (x,True) | Grid x _ <- gs ])


-- | Interface to a step.
data Command = RotAnti | RotClock
             | MoveLeft | MoveRight
             | MoveDownLeft | MoveDownRight
               deriving (Show,Eq,Ord)

data Result  = Error | Ok Step | Done Board
                deriving Show

-- | Apply a command and let us know what happened.
command :: Command -> Step -> Result
command cmd g =
  case cmd of
    RotAnti       -> checkDuplicates $ result $ rotateThePiece (-1) g
    RotClock      -> checkDuplicates $ result $ rotateThePiece 1 g
    MoveLeft      -> checkDuplicates $ result $ moveThePiece W g
    MoveRight     -> checkDuplicates $ result $ moveThePiece E g
    MoveDownLeft  -> reset $ result $ moveThePiece SW g
    MoveDownRight -> reset $ result $ moveThePiece SE g
  where
  result = maybe (Done (lockThePiece g)) Ok

  reset x =
    case x of
      Ok s -> Ok s { prevShapesRow = [ shape s ] }
      _    -> x

  checkDuplicates x =
    case x of
      Ok s | shape s `elem` prevShapesRow s -> Error
           | otherwise -> Ok s { prevShapesRow = shape s : prevShapesRow s }
      _ -> x


instance Export Step where
  toJSON g = object [ "board" .= board g, "piece" .= shape g ]




-- A whole game ----------------------------------------------------------------

-- | The state of a whole game.
data Game = Game
  { gameScore       :: !Integer
  , gamePrevLines   :: !Integer
  , gameMoves       :: !Integer
  , gameLocked      :: !Integer
  , gamePieces      :: [GridShape]
  , gameState       :: GameState
  }

data GameState = GameInProgress Step | GameOver GameOver

data GameOver = GameError GameError
              | GameOutOfShapes Board
              | GameFull Board

data GameError = InvalidCommand
               | CommandCausedError Command Step
               | CommandAfterEnd

-- | A new game with the given shapes and board.
newGameState :: [GridShape] -> Board -> (GameState, [GridShape])
newGameState ps b =
  case ps of
    []     -> (GameOver (GameOutOfShapes b), [])
    q : qs ->
      case newStep q b of
        Nothing -> (GameOver (GameFull b), ps)
        Just s  -> (GameInProgress s, qs)

-- | Setup a new game from a problem description.
newGameFromProblem :: Word32 -> Problem -> Game
newGameFromProblem seed Problem { .. } =
  Game { gameScore = 0
       , gamePrevLines = 0
       , gameMoves = 0
       , gameLocked = 0
       , .. }
  where
  (gameState, gamePieces) =
      newGameState (generate seed problemLen problemShapes)
                   (newBoardWith problemWidth problemHeight problemFilled)

-- | Execute a command on the game.
gameCommand :: Command -> Game -> Game
gameCommand cmd g =
  case gameState g of
    GameOver _ -> g
    GameInProgress s ->
      case command cmd s of
        Error  -> g { gameState = GameOver (GameError (CommandCausedError cmd s))
                    , gameScore = 0
                    }
        -- XXX: When playing as a human it is easier to treat errors as no-op.
        Ok s1  -> g { gameMoves = 1 + gameMoves g
                    , gameState = GameInProgress s1 }
        Done b ->
          let (l,b')  = checkLines b
              (gs,ps) = newGameState (gamePieces g) b'
              points  = fromIntegral (gridShapeSize (shape s)) +
                          100 * sum [ 1 .. l ]
              bouns   = if gamePrevLines g > 1
                          then ((gamePrevLines g - 1) * points) `div` 10
                          else 0
          in Game { gameScore       = gameScore g + points + bouns
                  , gamePrevLines   = l
                  , gameMoves       = 1 + gameMoves g
                  , gamePieces      = ps
                  , gameState       = gs
                  , gameLocked      = 1 + gameLocked g
                  }


playGame :: Word32 -> Problem -> [Maybe Command] -> Game
playGame seed p = go (newGameFromProblem seed p)
  where
  go g cs =
    case cs of
      []          -> g
      Nothing : _ -> g { gameState = GameOver (GameError InvalidCommand) }
      Just c : ds ->
        let g1 = gameCommand c g
        in case gameState g1 of
             GameOver {} -> case ds of
                              [] -> g1
                              _  -> g1 { gameState = GameOver (GameError CommandAfterEnd) }
             _           -> go g1 ds


gameStates :: Word32 -> Problem -> [Maybe Command] -> [Game]
gameStates seed p = go (newGameFromProblem seed p)
  where
  go g cs =
    case cs of
      []          -> []
      Nothing : _ -> [g { gameState = GameOver (GameError InvalidCommand) }]
      Just c : ds ->
        let g1 = gameCommand c g
        in case gameState g1 of
             GameOver {} -> case ds of
                              [] -> []
                              _  -> [g1 { gameState = GameOver (GameError CommandAfterEnd) }]
             _           -> g1 : go g1 ds



gameSteps :: [Game] -> [(Integer, Step)]
gameSteps gs = [ (gameScore g, s) | g <- gs
                                  , GameInProgress s <- [ gameState g ]  ]


exportCommand :: Command -> Text
exportCommand cmd =
  case cmd of
    RotAnti -> "turn counter-clockwise" :: Text
    RotClock -> "turn clockwise"
    MoveLeft -> "move W"
    MoveRight -> "move E"
    MoveDownLeft -> "move SW"
    MoveDownRight -> "move SE"



instance Export Game where
  toJSON Game { .. } =
    object [ "score"      .= gameScore
           , "prevLines"  .= gamePrevLines
           , "moves"      .= gameMoves
           , "locked"     .= gameLocked
           , "pieces"     .= gamePieces
           , "state"      .= gameState
           ]

instance Export GameState where
  toJSON g =
    case g of
      GameOver o       -> toJSON o
      GameInProgress s -> object [ tag "play", "step" .= s ]

instance Export GameOver where
  toJSON g =
    case g of
      GameError {}      -> object [ tag "error" ]
      GameOutOfShapes b -> object [ tag "end", "board" .= b ]
      GameFull b        -> object [ tag "full", "board" .= b ]


