{-# LANGUAGE RecordWildCards, OverloadedStrings, ParallelListComp#-}
module Solution where

import Hex
import Grid
import Problem
import Data.PQueue.Min
import Data.PQueue.Max
import Data.Set
import Data.Maybe
import Data.Vector
import MakeProblem
import Data.Word(Word32)
import Data.Map
import System.IO.Unsafe

data Ppair a = Ppair Int a deriving Show

instance Eq (Ppair a) where
  (Ppair p1 _) == (Ppair p2 _) = p1 == p2

instance Ord (Ppair a) where
  (Ppair p1 _) `compare` (Ppair p2 _) = p1 `compare` p2

increment :: Ord a => a -> a -> (Map a a, Map a Int)  -> (Map a a, Map a Int)
increment current neighbor (steps, map)  =
  let specScore = (map Data.Map.! current) + 1 in
  case (Data.Map.lookup neighbor map) of
  Just neighborScore -> if neighborScore < specScore
                        then (steps, map)
                        else (Data.Map.insert neighbor current steps,
                              Data.Map.insert neighbor specScore map)
  Nothing            -> (Data.Map.insert neighbor current steps,
                         Data.Map.insert neighbor specScore map)

astar' :: (Show a, Ord a) => MinQueue (Ppair a) -> Map a a -> Set a -> Map a Int -> (a -> Int) ->
          (a -> [a]) -> Maybe (Map a a) 
astar' open previous closed score heuristic nextf =
 -- unsafePerformIO (putStrLn (show (getMin open))) `seq`
  case getMin open of
   Nothing -> Nothing
   Just (Ppair priority state) -> if heuristic state == 0
                                  then Just (previous)
                                  else
                                    let currentscore = score Data.Map.! state
                                        prioritize = Prelude.map
                                                     (\x -> Ppair (heuristic x + currentscore) x) 
                                        steps = nextf state
                                        openSteps = Prelude.filter
                                                    (\x -> Data.Set.notMember x closed) steps
                                        open' = Prelude.foldr Data.PQueue.Min.insert
                                                (Data.PQueue.Min.deleteMin open)
                                                (prioritize openSteps)
                                        (previous', score') = Prelude.foldr (increment state)
                                                           (previous, score) steps 
                                        closed' = (Data.Set.insert state closed)
                                    in
                                    astar' open' previous' closed' score' heuristic nextf 

reconstructPath :: (Show a, Ord a) => Map a a -> a -> [a]
reconstructPath path goal =
 -- unsafePerformIO (putStrLn (show (goal))) `seq`
  case Data.Map.lookup goal path of
   Just prev -> goal : (reconstructPath path prev)
   Nothing -> [goal]
   

astar :: (Show a, Ord a) => a -> (a -> Int) -> (a -> [a]) -> a -> Maybe [a]
astar startState heuristic nextf goal =
  do
    pth <- astar' (Data.PQueue.Min.insert (Ppair (heuristic startState) startState)
                   Data.PQueue.Min.empty)
           Data.Map.empty Data.Set.empty (Data.Map.insert startState 0 Data.Map.empty)
           heuristic nextf
    return (reconstructPath pth goal)
    
moveThePieceNoCollide dir s =
  case (collideMoveCommand s) of
     Nothing -> moveThePiece dir s
     Just _ -> Nothing


getNext :: Board -> GridShape -> [GridShape]
getNext board shape =
  let step = Step shape board [] in
  Prelude.map Hex.shape
  (catMaybes [moveThePiece E step, moveThePiece SE step,
              moveThePiece SW step, moveThePiece W step
             {-rotateThePiece 1 step, rotateThePiece (-1) step-}]
              )


shortestPath :: Grid -> Grid -> Int
shortestPath (Grid x1 y1) (Grid x2 y2) =
  let dx = abs (x1 - x2) in
  let dy = abs (y1 - y2) in
  {-let mn = min dx dy in
  let mx = max dx dy in
  mn + (mx - mn) -} dx + dy

dirToCommand :: Dir -> Command
dirToCommand d =
  case d of
   E  -> MoveRight 
   SE -> MoveDownRight
   W  -> MoveLeft
   SW -> MoveDownLeft

--this will be wrong if they are more than a move apart
commandFromTo :: Grid -> Grid -> Command
commandFromTo (Grid x1 y1) (Grid x2 y2) =
  if y1 == y2
  then if x1 < x2
       then MoveRight
       else MoveLeft
  else if (even y1)
       then if x1 > x2
            then MoveDownLeft
            else MoveDownRight
       else if x1 < x2
            then MoveDownRight
            else MoveDownLeft

--expects shapes in order from start to end with single moves between
positionsToCommands :: [GridShape] -> [Command]
positionsToCommands (l : n : t) =
  let cmd = commandFromTo (center l) (center n) in
   cmd : (positionsToCommands (n : t)) 
positionsToCommands _ = []

instance (Ord GridShape) where
  GridShape a1 b1 `compare` GridShape a2 b2 =
    let acmp = compare a1 a2 in
    if acmp == EQ
    then compare b1 b2
    else acmp


pathTo :: Step -> Grid -> Maybe ([Command], Step)
pathTo Step{..} goal =
  do
    let (Grid x y) = goal
    positions <- (astar shape (\x -> shortestPath (center x) goal) (getNext board)
                 (teleportGridShape shape x y))
    return (positionsToCommands (Prelude.reverse positions),
            Step{shape = (teleportGridShape shape x y), board = board, ..})

moveGridBy ::  Int -> Int -> Grid -> Grid
moveGridBy dx dy (Grid x y) =
  Grid (x - dx) (y - dy)

--x and y will be the new center
teleportGridShape :: GridShape -> Int -> Int -> GridShape
teleportGridShape GridShape{center = (Grid cx cy), .. } x y =
  let dx = cx - x
      dy = cy - y
  in GridShape { center = Grid x y, members = Prelude.map (moveGridBy dx dy) members }

teleportStep :: Step -> Grid -> Step
teleportStep Step{..} (Grid x y) =
  let newShape = teleportGridShape shape x y in
  Step {shape = newShape, .. }

{-
getBottomForColumn' :: Step -> Int -> Int -> Maybe Grid
getBottomForColumn' Step { .. } col row =
  if (row < 0) then Nothing else
  let newshape = teleportGridShape shape col row 
      step = Step { shape = newshape, board = board } in 
  case (validate step) of
  Nothing -> getBottomForColumn' step col (row - 1)
  Just _ -> Just (Grid col row) 
  
getBottomForColumn :: Step -> Int -> Maybe Grid
getBottomForColumn step@Step{ board = Board{ ..}, .. } col  = getBottomForColumn' step col (boardHeight-1)

getBottomCenters :: Step -> [Grid]
getBottomCenters step@Step { board = Board { .. }, .. } =
  catMaybes (Prelude.map (getBottomForColumn step) [0 .. (boardWidth - 1)])
-}

canStick :: Step -> Bool
canStick step =
  case (validate step) of
   Nothing -> False
   Just _ ->
     case (collideMoveCommand step) of
     Nothing -> False
     Just _ -> -- unsafePerformIO (putStrLn (show (step))) `seq`
               True



getPossibleCenters :: Step -> [Grid]
getPossibleCenters step@Step { board = Board { .. }, .. } =
  let allGrids  = [ (Grid x y) | x <- [0 .. boardWidth ], y <- [0 .. boardHeight] ] 
      allSteps  = Prelude.map (teleportStep step) allGrids 
      zipped = Prelude.zip allGrids allSteps
      filtered = Prelude.filter (\(grd, stp) -> canStick stp) zipped in
   --unsafePerformIO (putStrLn (show (Prelude.map fst filtered))) `seq`
   Prelude.map fst filtered


boardQualityAt :: Int -> Int -> Board -> Int
boardQualityAt x y Board{ .. } =
  if (boardLayout Data.Vector.! y Data.Vector.! x)
  then y * y
  else 0 {-
    ( if y > 0 && (boardLayout Data.Vector.! (y - 1) Data.Vector.! x)
      then -y
      else 0 ) +
    ( if even y 
      then if x > 0 && (boardLayout Data.Vector.! y Data.Vector.! (x-1) )
           then y
           else 0
      else if (x < (boardWidth -1)) && (boardLayout Data.Vector.! y Data.Vector.! (x + 1))
           then y
           else 0 ) -}
      

boardQuality :: Board -> Int
boardQuality board@Board{..} =
 Prelude.sum [ (boardQualityAt c r board) | c <- [0 .. (boardWidth-1)], r <- [0 .. (boardHeight -1)] ]  

moveAndLock :: Step -> Grid -> (Grid, Board)
moveAndLock Step { .. } (grid@(Grid x y)) =
  let newshape = teleportGridShape shape x y in
  (grid, lockThePiece Step{ shape=newshape, .. }) 

makeAllBoards :: Step -> [(Grid, Board)]
makeAllBoards step =
  Prelude.map (moveAndLock step) (getPossibleCenters step)

prioritizeBoards :: Step -> MaxQueue (Ppair Grid)
prioritizeBoards step =
  let allBoards = makeAllBoards step 
      prioritized = (Prelude.map (\x -> let (grid, board) = x in
                                   Ppair (boardQuality board) grid)
                     allBoards) in
  Prelude.foldr Data.PQueue.Max.insert Data.PQueue.Max.empty prioritized

collideMoveCommand :: Step -> Maybe Command
collideMoveCommand s =
 case (command MoveDownRight s) of
 Done _ -> Just MoveDownRight
 _ -> case (command MoveDownLeft s) of
       Done _ -> Just MoveDownLeft
       _ -> case (command MoveRight s) of
             Done _ -> Just MoveRight
             _ -> case (command MoveLeft s) of
               Done _ -> Just MoveLeft
               _ -> Nothing

pickNextMove' :: MaxQueue (Ppair Grid) -> Step ->  [Command]
pickNextMove' q st@Step{..} =
  case (getMax q) of
   Just (Ppair _ bestMove) ->
     case (pathTo st bestMove) of
      Just (p, step)  -> case (collideMoveCommand step) of
                          Just c ->  unsafePerformIO (putStrLn (show (bestMove))) `seq`
                            p Prelude.++ [c]
                          Nothing -> error "No collision in location"
      Nothing -> pickNextMove' (Data.PQueue.Max.deleteMax q) st
   Nothing -> []

gameCommands :: [Command] -> Game -> Game
gameCommands cmds g@Game{ gameState = (GameInProgress step) , .. } =
  --unsafePerformIO (putStrLn (show (commandsToString (Prelude.map Just cmds)))) `seq`
  Prelude.foldl (\game command ->
                  --unsafePerformIO (putStrLn (show game)) `seq`
                  gameCommand command game) g cmds
gameCommands _ _ = error "too many commands"

{-instance Show Command where
  show RotAnti = "RotAnti"
  show RotClock = "RotClock"
  show MoveLeft = "MoveLeft"
  show MoveRight = "MoveRight"
  show MoveDownLeft = "MoveDownLeft"
  show MoveDownRight = "MoveDownRight" -}
                      
pickNextMove :: Step -> [Command]
pickNextMove step@Step{..} =
  let moveQueue = prioritizeBoards step in
  --unsafePerformIO (putStrLn (show (getMax moveQueue))) `seq`
  pickNextMove' moveQueue step

makeSolution' :: Game -> [Command] -> (Game, [Command])
makeSolution' game@Game{ gameState = (GameInProgress step) , .. } c =
  let commands = pickNextMove step in
  --unsafePerformIO (putStrLn (show (gameCommands commands game))) `seq`
  makeSolution' (gameCommands commands game) (c Prelude.++ commands)
makeSolution' game c = (game, c)

getGame :: Game -> Int -> Game
getGame game 0 = game
getGame game@Game{ gameState = (GameInProgress step) , .. } n =
  let commands = pickNextMove step in
  getGame (gameCommands commands game) (n-1)

getStepFromGame :: Game -> Step
getStepFromGame Game{ gameState = (GameInProgress step), .. } = step 

getStep :: Game -> Int -> Step
getStep game n =
  getStepFromGame (getGame game n)

makeSolution :: Game -> [Maybe Command] 
makeSolution g =
  let (_, ret) = makeSolution' g [] in
  Prelude.map Just ret

runit :: Problem -> Word32 -> ([Maybe Command], Integer, GameState, Integer)
runit problem seed =
  let game = newGameFromProblem seed problem 
      (Game{ .. }, cs) = makeSolution' game [] in
   ((Prelude.map Just cs), gameScore, gameState, gameMoves)
      
runit_score problem seed =
  let (_, i, s, m) = runit problem seed in
   (i, s, m)

runitString problem seed =
  let (c, _, _, _) = runit problem seed in
  commandsToString c

runitOutString problem seed =
  let (c, _, _, _) = runit problem seed in
  commandsToOutString c


commandsToString :: [Maybe Command] -> String
commandsToString (Just h : t) =
  let s = case h of
           MoveLeft -> 'h'
           MoveRight -> 'l'
           MoveDownLeft -> 'j'
           MoveDownRight -> 'k'
  in
   s : (commandsToString t)
commandsToString (Nothing : t) = commandsToString t
commandsToString [] = []

commandsToOutString :: [Maybe Command] -> String
commandsToOutString (Just h : t) =
  let s = case h of
           MoveLeft -> 'p'
           MoveRight -> 'e'
           MoveDownLeft -> 'i'
           MoveDownRight -> 'm'
  in
   s : (commandsToOutString t)
commandsToOutString (Nothing : t) = commandsToOutString t
commandsToOutString [] = []


p8 = problems Prelude.!! 6

s8 =
  let Game { gameState = (GameInProgress step), .. } = (newGameFromProblem 0 p8) in
   step

s2 = getStepFromGame (getGame g8 1)

g8 = newGameFromProblem 0 (p8s 5)

shape8 = GridShape { center = Grid 1 0, members = [ Grid 0 0, Grid 1 0, Grid 2 0 ] }

p8s n =
  let Problem{ problemLen = _, .. } = p8
      problemLen = n in
  Problem{..}

g02 = getGame g8 2
s01 = getStep g8 0

c = [Just MoveDownLeft,Just MoveDownLeft,Just MoveDownLeft,Just MoveDownLeft,Just MoveDownLeft,Just MoveDownLeft,Just MoveDownLeft,Just MoveDownRight,Just MoveDownLeft,Just MoveDownRight,Just MoveDownLeft,Just MoveDownLeft,Just MoveDownLeft,Just MoveDownLeft,Just MoveDownRight,Just MoveDownLeft,Just MoveDownRight,Just MoveDownLeft,Just MoveDownRight,Just MoveRight,Just MoveDownRight,Just MoveDownLeft,Just MoveDownRight,Just MoveDownLeft,Just MoveDownRight,Just MoveDownLeft,Just MoveDownRight,Just MoveDownLeft,Just MoveDownRight,Just MoveDownRight,Just MoveDownRight,Just MoveDownRight,Just MoveDownRight,Just MoveDownRight,Just MoveDownLeft,Just MoveDownLeft,Just MoveDownLeft,Just MoveDownRight]
