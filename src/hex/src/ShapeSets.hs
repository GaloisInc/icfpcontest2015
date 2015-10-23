{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module ShapeSets where

import Hex
import Problem
import Grid
import Export
import Sequences

import Data.List(nubBy)
import qualified Data.Vector as Vector


testShape :: [Shape]
testShape =
  [ Shape [ [d] | d <- directions ]
  , Shape [ [E,E,E] ]
  ]


directions :: [Dir]
directions = [ minBound .. maxBound ]

shapeSize :: Shape -> Int
shapeSize (Shape xs) = length xs

centerShape :: Shape -> GridShape
centerShape s = there { center = Grid (maxX `div` 2) (maxY `div` 2)}
  where
  there = normGridShape (shapeToGrid s (Grid 0 0))

  xs = [ a | Grid a _ <- members there ]
  ys = [ a | Grid _ a <- members there ]
  maxX = maximum xs
  maxY = maximum ys

shapeBitMap :: Shape -> (Grid, Board)
shapeBitMap gs = (center m, normGridShapeBitmap (members m))
  where
  m = normGridShape (shapeToGrid gs (Grid 0 0))

-- | "Snapshot" a bunch of grid coordinates into a board.
-- The board's dimensions are the smallest that will fit all points.
normGridShapeBitmap :: [Grid] -> Board
normGridShapeBitmap [] = Board { boardWidth = 0, boardHeight = 0
                               , boardLayout = Vector.empty }
normGridShapeBitmap gs =
  Board { boardWidth = maxX + 1
        , boardHeight = maxY + 1
        , boardLayout = Vector.fromList
                          [ Vector.fromList
                              [ Grid x y `elem` gs | x <- [ 0 .. maxX ] ]
                                                   | y <- [ 0 .. maxY ] ]
        }
  where
  xs = [ a | Grid a _ <- gs ]
  ys = [ a | Grid _ a <- gs ]
  maxX = maximum xs
  maxY = maximum ys



-- | Translate a grid-shape, so that its top-left corner is (0,0)
normGridShape :: GridShape -> GridShape
normGridShape gs =
  case members gs of
    [] -> gs
    ms
      | y > 0 -> normGridShape (moves NE y)
      | y < 0 -> normGridShape (moves SE (negate y))
      | x > 0 -> moves W x
      | x < 0 -> moves E (negate x)
      | otherwise -> gs
      where
      moves d n = GridShape { members = iterate (map (move d)) ms !! n
                            , center  = iterate (move d) (center gs) !! n
                            }

      xs = [ a | Grid a _ <- ms ]
      ys = [ a | Grid _ a <- ms ]

      x = minimum xs
      y = minimum ys






data ShapePiece = ShapePiece
  { pShape :: Shape
  , pPiece :: Board
  } deriving Show

toShapePiece :: Shape -> ShapePiece
toShapePiece s = ShapePiece { pShape = s, pPiece = snd (shapeBitMap s) }

sameShape :: ShapePiece -> ShapePiece -> Bool
sameShape s1 s2 = pPiece s1 == pPiece s2

sameShapeRot :: ShapePiece -> ShapePiece -> Bool
sameShapeRot s1 s2 = any (sameShape s1)
                   $ map toShapePiece
                   $ take 6
                   $ iterate (rotateShape 1)
                   $ pShape s2

pickOne :: [a] -> [ (a,[a]) ]
pickOne [] = []
pickOne (x : xs) = (x,xs) : [ (y, x:ys) | (y,ys) <- pickOne xs ]

nextLevel1 :: Shape -> [ShapePiece]
nextLevel1 s@(Shape xs) = nubBy sameShapeRot
               $ filter ((> lvl) . boardFilledSize . pPiece)
               [ toShapePiece (Shape ((d : p) : p : ps)) | (p,ps) <- pickOne xs
                                                 , d <- directions ]
  where lvl = shapeSize s

nextLevel :: [Shape] -> [Shape]
nextLevel = map pShape . nubBy sameShapeRot . concatMap nextLevel1

allLevels :: [ [GridShape] ]
allLevels = map (map centerShape) $ iterate nextLevel [Shape [[]]]

-- | Count how many places on the board are occupied.
boardFilledSize :: Board -> Int
boardFilledSize = Vector.foldl' row 0 . boardLayout
  where row n = Vector.foldl' count n
        count n b = if b then n + 1 else n



-- 1--3
lvl1 :: [Shape]
lvl1 = [ Shape [ [] ]
       , Shape [ [], [E] ]
       , Shape [ [], [W], [E] ]
       , Shape [ [], [W], [NE] ]
       , Shape [ [], [W], [NW] ]
       ]


-- 4
tetris :: [ Shape ]
tetris =
  [ Shape [ [], [W], [E], [E,E] ]
  , Shape [ [], [W], [E], [E,NE] ]
  , Shape [ [], [W], [E], [E,SE] ]
  , Shape [ [], [W], [E], [E,NW] ]
  , Shape [ [], [W], [E], [E,SW] ]
  , Shape [ [], [NW], [E], [E,NE] ]
  , Shape [ [], [W], [NE], [NE,E] ]
  , Shape [ [], [W], [SE], [SE,E] ]
  , Shape [ [], [W], [NW], [NE] ]
  , Shape [ [], [W], [NE], [SE] ]
  ]

fun :: [Shape]
fun = [ Shape i
      , Shape (i ++[ [NE,NE,E], [ NE, NE, E, E ], [SW,SW,E], [ SW, SW, E, E ] ])
      , Shape (i ++[ [NE,NE,E], [ NE, NE, E, E ], [E], [ E, E ] ])
      , Shape (i ++[ [NE,NE,E], [ NE, NE, E, E ], [E], [ E, E ], [E, E, NE ] ])
      ]
  where i = [ [], [NE], [NE,NE], [SW], [SW,SW] ]






