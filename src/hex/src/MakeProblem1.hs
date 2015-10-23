module Main where

import ShapeSets
import Problem
import Grid
import Sequences

import Data.Word(Word32)
import Data.List(nub)

main :: IO ()
main = mapM_ saveProblem [ p25, p26, p27, p28, p29, p30, p31, p32 ]

randomSeeds :: Int -> Int -> [ Word32 ]
randomSeeds pid num = map fromIntegral $ take num $ nub $ randomNumbers start
  where
  start = fromIntegral (randomNumbers 0xAAAA !! pid)




shapes3, shapes4, shapes5 :: [ GridShape ]
shapes3 = allLevels !! 2
shapes4 = allLevels !! 3
shapes5 = allLevels !! 4

line :: Int -> GridShape
line l = GridShape { center  = Grid (div l 2) 0
                   , members = [ Grid i 0 | i <- [ 0 .. l - 1 ] ]
                   }

spaced :: GridShape -> GridShape
spaced g = GridShape { center = scale (center g)
                     , members = map scale (members g)
                     }
  where
  scale (Grid x y) = Grid (2 * x) (2 * y)

sideBoard :: Int -> [ Grid ]
sideBoard h = [ Grid 1 x | x <- [ 0 .. h - 1 ] ]


moveBy :: Int -> Int -> Grid -> Grid
moveBy x y (Grid a b) = Grid (a + x) (b + y)




-- Just play normal tetris
p25 :: Problem
p25 = Problem
  { problemId       = 25
  , problemShapes   = shapes4 ++ shapes3 ++ shapes5
  , problemWidth    = 15
  , problemHeight   = 30
  , problemFilled   = []
  , problemLen      = 5000
  , problemSeeds    = randomSeeds 25 2
  }

-- Just play normal tetris, without scoring lines
p26 :: Problem
p26 = p25 { problemId = 26, problemFilled = sideBoard (problemHeight p25) }

-- Do teams take advantage of multi-lines?
p27 :: Problem
p27 = Problem
  { problemId     = 27
  , problemShapes = [ line 3 ]
  , problemWidth  = 9
  , problemHeight = 12
  , problemFilled = []
  , problemLen    = 500
  , problemSeeds  = [ 0 ]
  }


-- Using weird shapes: we insert spaces in the shapes!
p28 :: Problem
p28 = Problem
  { problemId       = 28
  , problemShapes   = map spaced (shapes4 ++ shapes3 ++ shapes5)
  , problemWidth    = 30
  , problemHeight   = 60
  , problemFilled   = []
  , problemLen      = 5000
  , problemSeeds    = randomSeeds 28 2
  }



-- Checks if teams do at least 1 look ahead
p29 :: Problem
p29 = Problem
  { problemId     = 29
  , problemShapes = [ line 1, line 4 ]
  , problemFilled = [ Grid x y
                      | x <- [ 0 .. 9 ]
                      , y <- [ 5 .. 8 ]
                      , x /= div y 2
                    ]
  , problemWidth  = 10
  , problemHeight = 9
  , problemLen    = 2
  , problemSeeds  = [ 0x1234 ]
  }



-- Checks if teams do 2 look ahead
p30 :: Problem
p30 = Problem
  { problemId     = 30
  , problemShapes = [ line 1, line 4 ]
  , problemFilled = [ Grid x y
                      | x <- [ 0 .. 9 ]
                      , y <- [ 5 .. 8 ]
                      , x /= div y 2
                    ]
  , problemWidth  = 10
  , problemHeight = 9
  , problemLen    = 3
  , problemSeeds  = [ 0x1235 ]
  }


p31 :: Problem
p31 = Problem
  { problemId = 31
  , problemShapes = GridShape { center = Grid 4 0
                              , members = [ Grid 0 0, Grid 1 0 ]
                              }
                  : shapes3 ++ shapes4

  , problemWidth = 40
  , problemHeight = 20
  , problemLen    = 5000
  , problemSeeds = randomSeeds 31 2
  , problemFilled = map (moveBy 0 5) [Grid 0 0, Grid 4 0, Grid 30 0, Grid 34 0, Grid 1 1, Grid 4 1, Grid 30 1, Grid 34 1, Grid 1 2, Grid 3 2, Grid 30 2, Grid 34 2, Grid 2 3, Grid 3 3, Grid 29 3, Grid 30 3, Grid 31 3, Grid 32 3, Grid 34 3, Grid 2 4, Grid 5 4, Grid 9 4, Grid 12 4, Grid 13 4, Grid 14 4, Grid 18 4, Grid 19 4, Grid 20 4, Grid 24 4, Grid 25 4, Grid 26 4, Grid 30 4, Grid 34 4, Grid 35 4, Grid 36 4, Grid 2 5, Grid 6 5, Grid 9 5, Grid 12 5, Grid 15 5, Grid 18 5, Grid 21 5, Grid 24 5, Grid 27 5, Grid 30 5, Grid 34 5, Grid 37 5, Grid 2 6, Grid 5 6, Grid 9 6, Grid 11 6, Grid 15 6, Grid 17 6, Grid 21 6, Grid 23 6, Grid 27 6, Grid 30 6, Grid 34 6, Grid 37 6, Grid 2 7, Grid 6 7, Grid 9 7, Grid 12 7, Grid 15 7, Grid 18 7, Grid 21 7, Grid 24 7, Grid 27 7, Grid 30 7, Grid 34 7, Grid 37 7, Grid 2 8, Grid 6 8, Grid 7 8, Grid 8 8, Grid 12 8, Grid 13 8, Grid 14 8, Grid 15 8, Grid 18 8, Grid 19 8, Grid 20 8, Grid 21 8, Grid 24 8, Grid 25 8, Grid 26 8, Grid 30 8, Grid 31 8, Grid 34 8, Grid 37 8, Grid 15 9, Grid 21 9, Grid 14 10, Grid 20 10, Grid 12 11, Grid 13 11, Grid 14 11, Grid 18 11, Grid 19 11, Grid 20 11]
  }


p32 :: Problem
p32 = Problem
  { problemId = 32
  , problemShapes = shapes4
  , problemWidth  = 17
  , problemHeight = 19
  , problemLen    = 2000
  , problemSeeds  = randomSeeds 32 2
  , problemFilled =
      map (moveBy 3 4) $
      [Grid 3 0, Grid 4 0, Grid 5 0, Grid 6 0, Grid 7 0, Grid 8 0, Grid 2 1, Grid 8 1, Grid 2 2, Grid 9 2, Grid 1 3, Grid 4 3, Grid 5 3, Grid 6 3, Grid 9 3, Grid 1 4, Grid 4 4, Grid 7 4, Grid 10 4, Grid 0 5, Grid 3 5, Grid 5 5, Grid 7 5, Grid 10 5, Grid 1 6, Grid 4 6, Grid 7 6, Grid 10 6, Grid 1 7, Grid 4 7, Grid 5 7, Grid 6 7, Grid 9 7, Grid 2 8, Grid 9 8, Grid 2 9, Grid 8 9, Grid 3 10, Grid 4 10, Grid 5 10, Grid 6 10, Grid 7 10, Grid 8 10]
  }
















