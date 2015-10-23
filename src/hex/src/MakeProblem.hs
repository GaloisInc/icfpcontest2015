{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main(main) where

import Debug.Trace

import Data.Word(Word32)

import Problem
import Grid
import Export
import Sequences

import Data.List(nub)
import Data.Maybe(catMaybes)
import Control.Monad(guard)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as JS
import System.FilePath((</>))
import System.Directory(createDirectoryIfMissing)

main :: IO ()
main =
  do createDirectoryIfMissing True problemDir
     mapM_ saveProblem problems

estimateSize :: Problem -> Integer
estimateSize Problem { .. } =
  fromIntegral problemHeight * 3 * fromIntegral problemLen * fromIntegral (length problemSeeds)

sanity = 500 * sum (map estimateSize problems)


randoms :: Int -> Int -> [Word32]
randoms p n = take n $ map fromIntegral $ randomNumbers
            $ fromIntegral $ nub (randomNumbers 0xAAAA) !! p

shiftCenter x y gs = gs { center = Grid (a+x) (b+y) }
    where Grid a b = center gs

-- dataVolume :: [Problem] -> Integer
-- dataVolume p = 
                     
problems :: [Problem]
problems =
  let problemId = 0
      problemPower = Nothing
      seedNum n = replicate n 0
      fixUp i p = p { problemId = i
                    , problemSeeds = if   all (0==) (problemSeeds p)
                                     then randoms i (length (problemSeeds p))
                                     else problemSeeds p
                    }
      unitShape = GridShape { center = Grid 0 0
                            , members = [Grid 0 0] }
      horizontalBarbell = GridShape { center = Grid 1 0
                                    , members = [Grid 0 0, Grid 2 0]}
      verticalBarbell = GridShape { center = Grid 0 1
                                  , members = [Grid 0 0, Grid 0 2]}
      twoOclockTriangle = GridShape { center = Grid 1 1
                                    , members = [Grid 2 0, Grid 0 1, Grid 2 2]}
      threeOclockTriange = GridShape { center = Grid 0 1
                                     , members = [Grid 0 0, Grid 1 1, Grid 0 2]}
      twoHorizRightPivot = GridShape { center = Grid 1 0
                                     , members = [Grid 0 0, Grid 1 0]}
      twoHorizLeftPivot = GridShape { center = Grid 0 0
                                    , members = [Grid 0 0, Grid 1 0]}
      nwSeRightPivot = GridShape { center = Grid 0 1
                                 , members = [Grid 0 0, Grid 0 1]}
      nwSeLeftPivot = GridShape { center = Grid 0 0
                                , members = [Grid 0 0, Grid 0 1]}
      horizLeftPivot = GridShape { center = Grid 0 0
                                 , members = [Grid 0 0, Grid 1 0, Grid 2 0]}
      horizMiddlePivot = GridShape { center = Grid 1 0
                                   , members = [Grid 0 0, Grid 1 0, Grid 2 0]}
      horizRightPivot = GridShape { center = Grid 2 0
                                   , members = [Grid 0 0, Grid 1 0, Grid 2 0]}
      gtTopPivot = GridShape { center = Grid 0 0
                             , members = [Grid 0 0, Grid 0 1, Grid 0 2]}
      gtMiddlePivot = GridShape { center = Grid 0 1
                                , members = [Grid 0 0, Grid 0 1, Grid 0 2]}
      gtBottomPivot = GridShape { center = Grid 0 2
                                , members = [Grid 0 0, Grid 0 1, Grid 0 2]}
      ltTopPivot = GridShape { center = Grid 1 0
                             , members = [Grid 1 0, Grid 0 1, Grid 1 2]}
      ltMiddlePivot = GridShape { center = Grid 1 1
                                , members = [Grid 1 0, Grid 0 1, Grid 1 2]}
      ltBottomPivot = GridShape { center = Grid 1 2
                                , members = [Grid 1 0, Grid 0 1, Grid 1 2]}
      cthulu = GridShape { center = Grid 9 5, members = [Grid 2 0, Grid 3 0, Grid 4 0, Grid 5 0, Grid 6 0, Grid 7 0, Grid 8 0, Grid 9 0, Grid 1 1, Grid 2 1, Grid 3 1, Grid 4 1, Grid 5 1, Grid 6 1, Grid 7 1, Grid 8 1, Grid 9 1, Grid 1 2, Grid 2 2, Grid 3 2, Grid 4 2, Grid 5 2, Grid 6 2, Grid 7 2, Grid 8 2, Grid 9 2, Grid 10 2, Grid 0 3, Grid 1 3, Grid 2 3, Grid 5 3, Grid 8 3, Grid 9 3, Grid 10 3, Grid 0 4, Grid 1 4, Grid 2 4, Grid 3 4, Grid 4 4, Grid 5 4, Grid 6 4, Grid 7 4, Grid 8 4, Grid 9 4, Grid 10 4, Grid 1 5, Grid 2 5, Grid 3 5, Grid 4 5, Grid 5 5, Grid 6 5, Grid 7 5, Grid 8 5, Grid 9 5, Grid 2 6, Grid 4 6, Grid 5 6, Grid 7 6, Grid 9 6, Grid 1 7, Grid 3 7, Grid 5 7, Grid 7 7, Grid 9 7, Grid 1 8, Grid 3 8, Grid 5 8, Grid 8 8, Grid 10 8, Grid 1 9, Grid 3 9, Grid 5 9, Grid 7 9, Grid 9 9]}
      triangle = GridShape { center = Grid 10 10, members = [Grid 3 0, Grid 2 1, Grid 3 1, Grid 2 2, Grid 3 2, Grid 4 2, Grid 1 3, Grid 2 3, Grid 3 3, Grid 4 3, Grid 1 4, Grid 2 4, Grid 3 4, Grid 4 4, Grid 5 4, Grid 0 5, Grid 1 5, Grid 2 5, Grid 3 5, Grid 4 5, Grid 5 5, Grid 0 6, Grid 1 6, Grid 2 6, Grid 3 6, Grid 4 6, Grid 5 6, Grid 6 6]}
      thickChevron = GridShape { center = Grid 10 8, members = [Grid 0 0, Grid 1 0, Grid 2 0, Grid 3 0, Grid 4 0, Grid 0 1, Grid 1 1, Grid 2 1, Grid 3 1, Grid 4 1, Grid 0 2, Grid 1 2, Grid 2 2, Grid 3 2, Grid 4 2, Grid 5 2, Grid 0 3, Grid 1 3, Grid 2 3, Grid 3 3, Grid 4 3, Grid 0 4, Grid 1 4, Grid 2 4, Grid 3 4, Grid 4 4]}
      lambda = GridShape { center = Grid 2 2
                         , members = [Grid 1 0, Grid 1 1, Grid 2 2, Grid 1 3, Grid 2 3, Grid 1 4, Grid 3 4, Grid 0 5, Grid 3 5]}
      fourDotsFar = GridShape { center = Grid 9 9, members = [Grid 0 0, Grid 5 0, Grid 0 6, Grid 5 6]}
      fourDotsCloseRight = GridShape { center = Grid 10 8, members = [Grid 0 0, Grid 3 0, Grid 0 3, Grid 2 3, Grid 3 3, Grid 0 4, Grid 3 4]}
      fourDotsCloseTop = GridShape { center = Grid 8 7, members = [Grid 0 0, Grid 3 0, Grid 0 4, Grid 3 4]}
      hexagonThree = GridShape { center = Grid 8 5, members = [Grid 1 0, Grid 2 0, Grid 3 0, Grid 1 1, Grid 4 1, Grid 0 2, Grid 4 2, Grid 1 3, Grid 4 3, Grid 1 4, Grid 2 4, Grid 3 4]}
      hexagonTwo = GridShape { center = Grid 8 6, members = [Grid 0 0, Grid 1 0, Grid 0 1, Grid 2 1, Grid 0 2, Grid 1 2]}
      hexagonFour = GridShape { center = Grid 8 7, members = [Grid 2 0, Grid 3 0, Grid 4 0, Grid 5 0, Grid 1 1, Grid 5 1, Grid 1 2, Grid 6 2, Grid 0 3, Grid 6 3, Grid 1 4, Grid 6 4, Grid 1 5, Grid 5 5, Grid 2 6, Grid 3 6, Grid 4 6, Grid 5 6]}
      thinChevron = GridShape { center = Grid 9 6, members = [Grid 0 0, Grid 0 1, Grid 1 2, Grid 0 3, Grid 0 4, Grid 1 4]}
      chai = GridShape { center = Grid 12 8, members = [Grid 0 0, Grid 1 0, Grid 5 0, Grid 0 1, Grid 1 1, Grid 2 1, Grid 5 1, Grid 0 2, Grid 1 2, Grid 2 2, Grid 3 2, Grid 5 2, Grid 6 2, Grid 7 2, Grid 8 2, Grid 9 2, Grid 10 2, Grid 11 2, Grid 12 2, Grid 1 3, Grid 2 3, Grid 3 3, Grid 4 3, Grid 6 3, Grid 7 3, Grid 8 3, Grid 9 3, Grid 10 3, Grid 11 3, Grid 12 3, Grid 13 3, Grid 2 4, Grid 3 4, Grid 4 4, Grid 5 4, Grid 6 4, Grid 11 4, Grid 12 4, Grid 2 5, Grid 3 5, Grid 4 5, Grid 5 5, Grid 6 5, Grid 11 5, Grid 12 5, Grid 4 6, Grid 5 6, Grid 6 6, Grid 11 6, Grid 12 6, Grid 4 7, Grid 5 7, Grid 6 7, Grid 7 7, Grid 11 7, Grid 12 7, Grid 5 8, Grid 6 8, Grid 11 8, Grid 12 8, Grid 6 9, Grid 7 9, Grid 11 9, Grid 12 9, Grid 5 10, Grid 6 10, Grid 7 10, Grid 11 10, Grid 12 10, Grid 5 11, Grid 6 11, Grid 11 11, Grid 12 11]}

      alien = GridShape
                { center = Grid 2 2
                , members = [ Grid 1 0, Grid 2 0, Grid 3 0, Grid 0 1
                            , Grid 1 1, Grid 2 1, Grid 3 1, Grid 0 2
                            , Grid 2 2, Grid 4 2, Grid 0 3, Grid 1 3
                            , Grid 2 3, Grid 3 3, Grid 0 4, Grid 2 4
                            , Grid 4 4]
                }
  in
  zipWith fixUp [ 0 .. ]
  [ Problem
      { problemWidth  = 10
      , problemHeight = 10
      , problemShapes = [ unitShape, horizontalBarbell,
                          verticalBarbell, twoOclockTriangle,
                          threeOclockTriange,
                          twoHorizLeftPivot, twoHorizRightPivot,
                          nwSeLeftPivot, nwSeRightPivot,
                          horizLeftPivot, horizMiddlePivot, horizRightPivot,
                          gtTopPivot, gtMiddlePivot, gtBottomPivot,
                          ltTopPivot, ltMiddlePivot, ltBottomPivot
                        ]
      , problemFilled = []
      , problemLen    = 100
      , problemSeeds  = seedNum 1
      , ..
      }

    -- Ei!
    , Problem {
        problemWidth = 15
      , problemHeight = 15
      , problemShapes = [ unitShape ]
      , problemSeeds = seedNum 1
      , problemLen   = 100
      , problemFilled = [Grid (x+2) (y+4) | Grid x y <- [Grid 0 0, Grid 1 0, Grid 2 0, Grid 3 0, Grid 4 0, Grid 9 0, Grid 0 1, Grid 6 1, Grid 9 1, Grid 0 2, Grid 9 2, Grid 0 3, Grid 1 3, Grid 2 3, Grid 6 3, Grid 9 3, Grid 0 4, Grid 7 4, Grid 9 4, Grid 0 5, Grid 6 5, Grid 0 6, Grid 1 6, Grid 2 6, Grid 3 6, Grid 4 6, Grid 7 6, Grid 9 6]]
      , ..
    }

  , Problem
  { problemWidth  = 15
  , problemHeight = 30
  , problemShapes =
      [GridShape {center = Grid 1 2, members = [Grid 2 0,Grid 1 1,Grid 1 2,Grid 0 3,Grid 0 4]},GridShape {center = Grid 1 1, members = [Grid 3 0,Grid 2 0,Grid 1 1,Grid 1 2,Grid 0 3]},GridShape {center = Grid 1 1, members = [Grid 2 1,Grid 2 0,Grid 1 1,Grid 1 2,Grid 0 3]},GridShape {center = Grid 1 1, members = [Grid 1 0,Grid 2 0,Grid 1 1,Grid 1 2,Grid 0 3]},GridShape {center = Grid 0 2, members = [Grid 1 0,Grid 1 1,Grid 1 2,Grid 0 3,Grid 0 4]},GridShape {center = Grid 1 1, members = [Grid 2 2,Grid 1 1,Grid 2 0,Grid 1 2,Grid 0 3]},GridShape {center = Grid 1 1, members = [Grid 3 0,Grid 2 1,Grid 1 1,Grid 1 2,Grid 0 3]},GridShape {center = Grid 1 1, members = [Grid 3 0,Grid 2 0,Grid 1 0,Grid 0 1,Grid 0 2]},GridShape {center = Grid 1 1, members = [Grid 2 1,Grid 2 0,Grid 1 0,Grid 0 1,Grid 0 2]},GridShape {center = Grid 1 1, members = [Grid 1 1,Grid 2 0,Grid 1 0,Grid 0 1,Grid 0 2]},GridShape {center = Grid 1 1, members = [Grid 0 0,Grid 1 0,Grid 2 0,Grid 0 1,Grid 0 2]},GridShape {center = Grid 1 1, members = [Grid 1 0,Grid 1 1,Grid 2 1,Grid 1 2,Grid 0 3]},GridShape {center = Grid 1 1, members = [Grid 1 2,Grid 0 1,Grid 2 0,Grid 1 0,Grid 0 2]},GridShape {center = Grid 1 1, members = [Grid 0 1,Grid 1 1,Grid 3 0,Grid 2 0,Grid 1 2]},GridShape {center = Grid 1 1, members = [Grid 0 3,Grid 0 2,Grid 2 0,Grid 1 0,Grid 0 1]},GridShape {center = Grid 1 1, members = [Grid 0 2,Grid 1 2,Grid 3 0,Grid 2 0,Grid 1 1]},GridShape {center = Grid 1 1, members = [Grid 2 1,Grid 1 1,Grid 1 0,Grid 0 1,Grid 0 2]},GridShape {center = Grid 1 1, members = [Grid 2 2,Grid 1 1,Grid 1 0,Grid 0 1,Grid 0 2]},GridShape {center = Grid 0 1, members = [Grid 1 2,Grid 1 1,Grid 1 0,Grid 0 1,Grid 0 2]},GridShape {center = Grid 0 1, members = [Grid 0 0,Grid 1 0,Grid 1 1,Grid 0 1,Grid 0 2]},GridShape {center = Grid 1 1, members = [Grid 0 1,Grid 1 1,Grid 2 1,Grid 2 0,Grid 1 2]},GridShape {center = Grid 0 1, members = [Grid 0 3,Grid 0 2,Grid 1 1,Grid 1 0,Grid 0 1]},GridShape {center = Grid 0 1, members = [Grid 1 0,Grid 0 1,Grid 1 1,Grid 1 2,Grid 0 3]},GridShape {center = Grid 0 1, members = [Grid 0 3,Grid 0 2,Grid 0 0,Grid 1 0,Grid 0 1]},GridShape {center = Grid 0 2, members = [Grid 1 0,Grid 0 1,Grid 1 2,Grid 0 3,Grid 0 4]},GridShape {center = Grid 0 1, members = [Grid 0 0,Grid 1 0,Grid 1 1,Grid 1 2,Grid 0 3]},GridShape {center = Grid 0 2, members = [Grid 1 4,Grid 0 3,Grid 1 0,Grid 1 1,Grid 1 2]},GridShape {center = Grid 1 1, members = [Grid 3 0,Grid 2 0,Grid 1 1,Grid 0 1,Grid 0 2]},GridShape {center = Grid 1 1, members = [Grid 2 0,Grid 2 1,Grid 2 2,Grid 1 2,Grid 0 3]},GridShape {center = Grid 1 1, members = [Grid 2 2,Grid 1 1,Grid 2 0,Grid 0 1,Grid 0 2]},GridShape {center = Grid 1 1, members = [Grid 2 2,Grid 2 1,Grid 2 0,Grid 1 0,Grid 0 1]},GridShape {center = Grid 1 1, members = [Grid 0 0,Grid 0 1,Grid 2 2,Grid 1 1,Grid 0 2]},GridShape {center = Grid 1 0, members = [Grid 0 1,Grid 1 1,Grid 3 1,Grid 3 0,Grid 2 0]}]
  , problemSeeds = seedNum 10
  , problemLen = 100
  , problemFilled = []
  , ..
}

  -- Ya Ya
  , Problem
  { problemWidth  = 30
  , problemHeight = 20
  , problemFilled = map (moveBy 2 5) [ Grid 0 0, Grid 1 0, Grid 2 0, Grid 3 0, Grid 9 0, Grid 14 0, Grid 15 0, Grid 16 0, Grid 17 0, Grid 23 0, Grid 2 1, Grid 9 1, Grid 16 1, Grid 23 1, Grid 2 2, Grid 9 2, Grid 16 2, Grid 23 2, Grid 2 3, Grid 9 3, Grid 16 3, Grid 23 3, Grid 2 4, Grid 5 4, Grid 6 4, Grid 9 4, Grid 16 4, Grid 19 4, Grid 20 4, Grid 23 4, Grid 2 5, Grid 5 5, Grid 7 5, Grid 16 5, Grid 19 5, Grid 21 5, Grid 0 6, Grid 1 6, Grid 2 6, Grid 3 6, Grid 5 6, Grid 6 6, Grid 7 6, Grid 9 6, Grid 14 6, Grid 15 6, Grid 16 6, Grid 17 6, Grid 19 6, Grid 20 6, Grid 21 6, Grid 23 6]
  , problemLen = 100
  , problemShapes = GridShape { center = Grid 1 0
                                , members = [ Grid 0 0, Grid 2 0 ]
                                }
                  : tet3 ++ tet4
  , problemSeeds = seedNum 5
  , ..
  }


  , Problem
      { problemWidth  = 10
      , problemHeight = 15
      , problemFilled = []
      , problemLen    = 200
      , problemSeeds  = seedNum 50
      , problemShapes =
          [ GridShape
              { center = Grid 1 0
              , members = [ Grid 1 0 , Grid 0 0 , Grid 2 0 , Grid 3 0 ]
              }
          , GridShape
              { center = Grid 1 1
              , members = [ Grid 1 1 , Grid 0 1 , Grid 2 1 , Grid 3 0 ]
              }
          , GridShape
              { center = Grid 1 0
              , members = [ Grid 1 0 , Grid 0 0 , Grid 2 0 , Grid 2 1 ]
              }
          , GridShape
              { center = Grid 1 1
              , members = [ Grid 1 1 , Grid 0 1 , Grid 2 1 , Grid 2 0 ]
              }
          , GridShape
              { center = Grid 1 0
              , members = [ Grid 1 0 , Grid 0 0 , Grid 2 0 , Grid 1 1 ]
              }
          , GridShape
              { center = Grid 0 1
              , members = [ Grid 0 1 , Grid 0 0 , Grid 1 1 , Grid 2 0 ]
              }
          , GridShape
              { center = Grid 1 1
              , members = [ Grid 1 1 , Grid 0 1 , Grid 2 0 , Grid 3 0 ]
              }
          , GridShape
              { center = Grid 1 0
              , members = [ Grid 1 0 , Grid 0 0 , Grid 1 1 , Grid 2 1 ]
              }
          , GridShape
              { center = Grid 1 1
              , members = [ Grid 1 1 , Grid 0 1 , Grid 1 0 , Grid 2 0 ]
              }
          , GridShape
              { center = Grid 1 1
              , members = [ Grid 1 1 , Grid 0 1 , Grid 2 0 , Grid 2 2 ]
              }
          ]
      , ..
      }



    -- R'yl
  , Problem
  { problemWidth = 30
  , problemHeight = 20
  , problemSeeds = seedNum 10
  , problemFilled = map (moveBy 2 6) [Grid 0 0, Grid 1 0, Grid 2 0, Grid 3 0, Grid 6 0, Grid 8 0, Grid 20 0, Grid 0 1, Grid 3 1, Grid 5 1, Grid 8 1, Grid 19 1, Grid 0 2, Grid 4 2, Grid 6 2, Grid 8 2, Grid 20 2, Grid 0 3, Grid 1 3, Grid 2 3, Grid 3 3, Grid 8 3, Grid 10 3, Grid 13 3, Grid 15 3, Grid 16 3, Grid 17 3, Grid 19 3, Grid 0 4, Grid 3 4, Grid 8 4, Grid 10 4, Grid 13 4, Grid 15 4, Grid 18 4, Grid 20 4, Grid 21 4, Grid 22 4, Grid 0 5, Grid 3 5, Grid 8 5, Grid 10 5, Grid 12 5, Grid 14 5, Grid 15 5, Grid 16 5, Grid 17 5, Grid 19 5, Grid 22 5, Grid 0 6, Grid 4 6, Grid 8 6, Grid 11 6, Grid 12 6, Grid 15 6, Grid 20 6, Grid 23 6, Grid 0 7, Grid 4 7, Grid 8 7, Grid 11 7, Grid 15 7, Grid 16 7, Grid 17 7, Grid 19 7, Grid 22 7, Grid 11 8, Grid 10 9]
  , problemShapes = GridShape { center = Grid 1 0
                                , members = [ Grid 0 0, Grid 2 0 ]
                                }
                  : tet3 ++ tet4
  , problemLen = 100
  , ..
  }


  , Problem
      { problemWidth  = 10
      , problemHeight = 10
      , problemShapes = [ unitShape

                        , GridShape { center = Grid 0 0
                                    , members = [ Grid 0 0, Grid 1 0 ]
                                    }

                        , GridShape { center = Grid 1 0
                                    , members = [ Grid 0 0, Grid 1 0, Grid 2 0 ]
                                    }

                        , GridShape { center = Grid 0 0
                                    , members = [ Grid 0 0, Grid 1 0, Grid 0 1 ]
                                    }

                        , GridShape { center = Grid 1 0
                                    , members = [ Grid 0 0, Grid 1 0, Grid 1 1 ]
                                    }
                        ]
      , problemFilled = []
      , problemLen    = 150
      , problemSeeds  = seedNum 50
      , ..
      }

    -- Yugoth
  , Problem
  { problemWidth = 40
  , problemHeight = 20
  , problemSeeds = seedNum 5
  , problemFilled = map (moveBy 0 5) [Grid 0 0, Grid 4 0, Grid 30 0, Grid 34 0, Grid 1 1, Grid 4 1, Grid 30 1, Grid 34 1, Grid 1 2, Grid 3 2, Grid 30 2, Grid 34 2, Grid 2 3, Grid 3 3, Grid 29 3, Grid 30 3, Grid 31 3, Grid 32 3, Grid 34 3, Grid 2 4, Grid 5 4, Grid 9 4, Grid 12 4, Grid 13 4, Grid 14 4, Grid 18 4, Grid 19 4, Grid 20 4, Grid 24 4, Grid 25 4, Grid 26 4, Grid 30 4, Grid 34 4, Grid 35 4, Grid 36 4, Grid 2 5, Grid 6 5, Grid 9 5, Grid 12 5, Grid 15 5, Grid 18 5, Grid 21 5, Grid 24 5, Grid 27 5, Grid 30 5, Grid 34 5, Grid 37 5, Grid 2 6, Grid 5 6, Grid 9 6, Grid 11 6, Grid 15 6, Grid 17 6, Grid 21 6, Grid 23 6, Grid 27 6, Grid 30 6, Grid 34 6, Grid 37 6, Grid 2 7, Grid 6 7, Grid 9 7, Grid 12 7, Grid 15 7, Grid 18 7, Grid 21 7, Grid 24 7, Grid 27 7, Grid 30 7, Grid 34 7, Grid 37 7, Grid 2 8, Grid 6 8, Grid 7 8, Grid 8 8, Grid 12 8, Grid 13 8, Grid 14 8, Grid 15 8, Grid 18 8, Grid 19 8, Grid 20 8, Grid 21 8, Grid 24 8, Grid 25 8, Grid 26 8, Grid 30 8, Grid 31 8, Grid 34 8, Grid 37 8, Grid 15 9, Grid 21 9, Grid 14 10, Grid 20 10, Grid 12 11, Grid 13 11, Grid 14 11, Grid 18 11, Grid 19 11, Grid 20 11]

  , problemShapes = GridShape { center = Grid 1 0
                              , members = [ Grid 0 0, Grid 2 0 ]
                              }
                  : tet3 ++ tet4
  , problemLen = 100
  , ..
  }







  , Problem
      { problemWidth  = 10
      , problemHeight = 8
      , problemShapes = GridShape { center  = Grid 1 0
                                  , members = [ Grid i 0 | i <- [ 0 .. 3 ] ]
                                  }
                      : replicate 7 GridShape { center = Grid 0 0
                                              , members = [ Grid 0 0 ] }
      , problemFilled = []
      , problemLen    = 400
      , problemSeeds  = seedNum 10
      , ..
      }

  , Problem
      { problemWidth  = 10
      , problemHeight = 8
      , problemShapes = replicate 1
                          GridShape { center  = Grid 1 0
                                    , members = [ Grid i 0 | i <- [ 0 .. 3 ] ]
                                    }
                      ++ replicate 3
                          GridShape { center  = Grid 1 0
                                    , members = [ Grid i 0 | i <- [ 0 .. 2 ] ]
                                    }
      , problemFilled = [ Grid i 3 | i <- [ 0 .. 5 ] ]
      , problemLen    = 400
      , problemSeeds  = seedNum 5
      , ..
      }

  , Problem
      { problemWidth  = 10
      , problemHeight = 7
      , problemShapes = replicate 1 GridShape { center = Grid 0 0
                                              , members = [ Grid 2 0, Grid 3 0 ]
                                              }
      , problemFilled = [ Grid x 1 | x <- take 5 [ 0, 2 .. ] ] ++
                        [ Grid x 5 | x <- [ 1 .. 8 ] ]
      , problemLen    = 100
      , problemSeeds  = seedNum 1
      , ..
      }

  , Problem
      { problemWidth  = 10
      , problemHeight = 10
      , problemShapes = concat
                         [ replicate 2 GridShape { center = Grid 2 0
                                                 , members = [ Grid 0 0 ]
                                                 }
                        , [ GridShape { center = Grid 1 1
                                    , members = map (`Grid` 0) [ 1 .. 2 ] ++
                                                [ Grid 0 1, Grid 2 1 ] ++
                                                map (`Grid` 2) [ 1 .. 2 ]
                                    } ]
                        ]
      , problemLen  = 50
      , problemSeeds = seedNum 5
      , problemFilled = []
      , ..
      }

  , Problem
      { problemWidth  = 15
      , problemHeight = 20
      , problemShapes = concat
                         [ replicate 8 GridShape { center = Grid 4 0
                                                 , members = [ Grid 0 0 ]
                                                 }
                        , replicate 2 GridShape { center = Grid 3 5
                                       , members = map (`Grid` 0) [ 1 .. 2 ] ++
                                                   [ Grid 0 1, Grid 2 1 ] ++
                                                   map (`Grid` 2) [ 1 .. 2 ] }
                        , replicate 1
                           GridShape { center = Grid 8 6
                                    , members = [Grid 1 0, Grid 2 0, Grid 3 0, Grid 0 1, Grid 3 1, Grid 0 2, Grid 4 2, Grid 0 3, Grid 3 3, Grid 1 4, Grid 2 4, Grid 3 4]}
                        ]
      , problemLen  = 100
      , problemSeeds  = seedNum 10
      , problemFilled = []
      , ..
      }

  , Problem
      { problemWidth = 15
      , problemHeight = 20
      , problemShapes = [alien]
      , problemSeeds = seedNum 1
      , problemLen   = 100
      , problemFilled = []
      , ..
      },

  -- pentagram
  Problem {
        problemWidth = 50
      , problemHeight = 50
      , problemShapes = [ unitShape, horizontalBarbell, twoOclockTriangle ]
      , problemSeeds = seedNum 1
      , problemLen   = 500
      , problemFilled = [Grid (x+8) (y+5) | Grid x y <- [Grid 13 0, Grid 14 0, Grid 15 0, Grid 16 0, Grid 17 0, Grid 18 0, Grid 19 0, Grid 20 0, Grid 11 1, Grid 12 1, Grid 13 1, Grid 21 1, Grid 22 1, Grid 23 1, Grid 9 2, Grid 10 2, Grid 23 2, Grid 24 2, Grid 8 3, Grid 9 3, Grid 25 3, Grid 7 4, Grid 8 4, Grid 25 4, Grid 26 4, Grid 6 5, Grid 7 5, Grid 8 5, Grid 9 5, Grid 10 5, Grid 25 5, Grid 26 5, Grid 27 5, Grid 28 5, Grid 4 6, Grid 5 6, Grid 8 6, Grid 10 6, Grid 11 6, Grid 24 6, Grid 25 6, Grid 28 6, Grid 4 7, Grid 8 7, Grid 12 7, Grid 13 7, Grid 23 7, Grid 24 7, Grid 26 7, Grid 29 7, Grid 30 7, Grid 3 8, Grid 8 8, Grid 13 8, Grid 21 8, Grid 22 8, Grid 25 8, Grid 30 8, Grid 3 9, Grid 9 9, Grid 14 9, Grid 15 9, Grid 20 9, Grid 21 9, Grid 25 9, Grid 31 9, Grid 2 10, Grid 9 10, Grid 15 10, Grid 16 10, Grid 18 10, Grid 19 10, Grid 24 10, Grid 31 10, Grid 2 11, Grid 9 11, Grid 17 11, Grid 18 11, Grid 24 11, Grid 32 11, Grid 1 12, Grid 9 12, Grid 16 12, Grid 18 12, Grid 24 12, Grid 32 12, Grid 1 13, Grid 10 13, Grid 15 13, Grid 16 13, Grid 19 13, Grid 20 13, Grid 24 13, Grid 33 13, Grid 1 14, Grid 10 14, Grid 13 14, Grid 14 14, Grid 20 14, Grid 21 14, Grid 23 14, Grid 33 14, Grid 1 15, Grid 11 15, Grid 12 15, Grid 13 15, Grid 22 15, Grid 23 15, Grid 33 15, Grid 1 16, Grid 10 16, Grid 11 16, Grid 22 16, Grid 23 16, Grid 33 16, Grid 1 17, Grid 10 17, Grid 11 17, Grid 23 17, Grid 24 17, Grid 25 17, Grid 33 17, Grid 0 18, Grid 8 18, Grid 9 18, Grid 11 18, Grid 22 18, Grid 25 18, Grid 33 18, Grid 1 19, Grid 8 19, Grid 12 19, Grid 22 19, Grid 26 19, Grid 27 19, Grid 33 19, Grid 0 20, Grid 6 20, Grid 7 20, Grid 12 20, Grid 21 20, Grid 27 20, Grid 28 20, Grid 33 20, Grid 1 21, Grid 5 21, Grid 6 21, Grid 12 21, Grid 21 21, Grid 29 21, Grid 33 21, Grid 1 22, Grid 3 22, Grid 4 22, Grid 12 22, Grid 21 22, Grid 29 22, Grid 30 22, Grid 32 22, Grid 1 23, Grid 3 23, Grid 13 23, Grid 21 23, Grid 31 23, Grid 32 23, Grid 1 24, Grid 2 24, Grid 3 24, Grid 4 24, Grid 5 24, Grid 6 24, Grid 7 24, Grid 8 24, Grid 9 24, Grid 10 24, Grid 11 24, Grid 12 24, Grid 13 24, Grid 14 24, Grid 15 24, Grid 16 24, Grid 17 24, Grid 18 24, Grid 19 24, Grid 20 24, Grid 21 24, Grid 22 24, Grid 23 24, Grid 24 24, Grid 25 24, Grid 26 24, Grid 27 24, Grid 28 24, Grid 29 24, Grid 30 24, Grid 31 24, Grid 32 24, Grid 2 25, Grid 13 25, Grid 20 25, Grid 32 25, Grid 2 26, Grid 13 26, Grid 20 26, Grid 32 26, Grid 3 27, Grid 14 27, Grid 20 27, Grid 32 27, Grid 3 28, Grid 14 28, Grid 19 28, Grid 30 28, Grid 31 28, Grid 4 29, Grid 14 29, Grid 19 29, Grid 31 29, Grid 4 30, Grid 14 30, Grid 18 30, Grid 30 30, Grid 5 31, Grid 15 31, Grid 19 31, Grid 29 31, Grid 30 31, Grid 5 32, Grid 15 32, Grid 18 32, Grid 28 32, Grid 6 33, Grid 7 33, Grid 16 33, Grid 18 33, Grid 27 33, Grid 28 33, Grid 7 34, Grid 16 34, Grid 17 34, Grid 26 34, Grid 8 35, Grid 9 35, Grid 10 35, Grid 16 35, Grid 17 35, Grid 24 35, Grid 25 35, Grid 26 35, Grid 10 36, Grid 11 36, Grid 12 36, Grid 16 36, Grid 21 36, Grid 22 36, Grid 23 36, Grid 13 37, Grid 14 37, Grid 15 37, Grid 16 37, Grid 17 37, Grid 18 37, Grid 19 37, Grid 20 37, Grid 21 37]]
      , ..
      }

  , let s1 = GridShape
              { center = Grid 1 1
              , members = [ Grid 0 0, Grid 2 0
                          , Grid 0 2, Grid 2 2
                          ]
              }
    in Problem
    { problemWidth  = 15
    , problemHeight = 20
    , problemShapes = [s1]
    , problemLen    = 100
    , problemFilled = []
    , problemSeeds  = seedNum 1
    , ..
    }

   , let nums = map (\x ->  mod x 5 == 1) (randoms 5 (15 * 20))
     in Problem
    { problemWidth  = 15
    , problemHeight = 20
    , problemShapes =   [ GridShape { center = Grid 0 0
                                    , members = [ Grid 0 0 ]
                                    }

                        , GridShape { center = Grid 0 0
                                    , members = [ Grid 0 0, Grid 1 0 ]
                                    }

                        , GridShape { center = Grid 1 0
                                    , members = [ Grid 0 0, Grid 1 0, Grid 2 0 ]
                                    }

                        , GridShape { center = Grid 0 0
                                    , members = [ Grid 0 0, Grid 1 0, Grid 0 1 ]
                                    }

                        , GridShape { center = Grid 1 0
                                    , members = [ Grid 0 0, Grid 1 0, Grid 1 1 ]
                                    }
                        ]
 

    , problemLen    = 100
    , problemFilled = catMaybes
                    $ zipWith (\a b -> guard a >> Just b)
                      nums
                      [ Grid x y | x <- [ 0 .. 14 ], y <- [ 5 .. 19 ] ]
    , problemSeeds  = seedNum 1
    , ..
    },

    let singleHolesMap = [Grid x (y+7) | Grid x y <- [Grid 0 1, Grid 1 1, Grid 2 1, Grid 3 1, Grid 5 1, Grid 6 1, Grid 7 1, Grid 8 1, Grid 9 1, Grid 10 1, Grid 11 1, Grid 12 1, Grid 13 1, Grid 14 1, Grid 0 2, Grid 1 2, Grid 2 2, Grid 3 2, Grid 4 2, Grid 5 2, Grid 6 2, Grid 7 2, Grid 8 2, Grid 9 2, Grid 10 2, Grid 11 2, Grid 12 2, Grid 14 2, Grid 0 3, Grid 2 3, Grid 3 3, Grid 4 3, Grid 5 3, Grid 6 3, Grid 7 3, Grid 8 3, Grid 9 3, Grid 10 3, Grid 11 3, Grid 12 3, Grid 13 3, Grid 14 3, Grid 0 4, Grid 1 4, Grid 2 4, Grid 3 4, Grid 4 4, Grid 5 4, Grid 7 4, Grid 8 4, Grid 9 4, Grid 10 4, Grid 11 4, Grid 12 4, Grid 13 4, Grid 14 4, Grid 0 5, Grid 1 5, Grid 2 5, Grid 4 5, Grid 5 5, Grid 6 5, Grid 7 5, Grid 8 5, Grid 9 5, Grid 10 5, Grid 11 5, Grid 12 5, Grid 13 5, Grid 14 5, Grid 0 6, Grid 1 6, Grid 2 6, Grid 3 6, Grid 4 6, Grid 5 6, Grid 6 6, Grid 7 6, Grid 8 6, Grid 9 6, Grid 10 6, Grid 11 6, Grid 13 6, Grid 14 6, Grid 0 7, Grid 1 7, Grid 2 7, Grid 3 7, Grid 4 7, Grid 5 7, Grid 6 7, Grid 7 7, Grid 8 7, Grid 9 7, Grid 10 7, Grid 11 7, Grid 12 7, Grid 13 7 ]]
    in 
    Problem {
        problemWidth = 15
      , problemHeight = 15
      , problemShapes = [ unitShape ]
      , problemSeeds = seedNum 1
      , problemLen   = 100
      , problemFilled = singleHolesMap
      , ..
    },


    -- R'lyeh
    Problem {
        problemWidth = 30
      , problemHeight = 30
      , problemShapes = concat [ replicate 10 unitShape
                        , replicate 10 GridShape { center = Grid 3 0
                                    , members = [ Grid 0 0 ]
                                    }
                        , replicate 1 alien , replicate 5 lambda ]

      , problemSeeds = seedNum 1
      , problemLen   = 1000
      , problemFilled = [Grid x (y+6) | Grid x y <- [Grid 9 0, Grid 12 0, Grid 6 1, Grid 7 1, Grid 8 1, Grid 9 1, Grid 10 1, Grid 11 1, Grid 13 1, Grid 15 1, Grid 4 2, Grid 5 2, Grid 6 2, Grid 7 2, Grid 8 2, Grid 9 2, Grid 10 2, Grid 11 2, Grid 12 2, Grid 13 2, Grid 14 2, Grid 15 2, Grid 5 3, Grid 6 3, Grid 8 3, Grid 10 3, Grid 11 3, Grid 12 3, Grid 13 3, Grid 14 3, Grid 15 3, Grid 16 3, Grid 21 3, Grid 22 3, Grid 24 3, Grid 26 3, Grid 27 3, Grid 7 4, Grid 8 4, Grid 9 4, Grid 10 4, Grid 11 4, Grid 16 4, Grid 23 4, Grid 24 4, Grid 25 4, Grid 26 4, Grid 27 4, Grid 28 4, Grid 29 4, Grid 24 5, Grid 26 5, Grid 27 5, Grid 10 15, Grid 14 15, Grid 18 15, Grid 10 16, Grid 15 16, Grid 19 16, Grid 3 17, Grid 9 17, Grid 10 17, Grid 14 17, Grid 19 17, Grid 26 17, Grid 3 18, Grid 4 18, Grid 9 18, Grid 10 18, Grid 14 18, Grid 15 18, Grid 19 18, Grid 20 18, Grid 23 18, Grid 26 18, Grid 2 19, Grid 3 19, Grid 4 19, Grid 8 19, Grid 9 19, Grid 14 19, Grid 19 19, Grid 20 19, Grid 22 19, Grid 23 19, Grid 25 19, Grid 26 19, Grid 2 20, Grid 3 20, Grid 4 20, Grid 5 20, Grid 8 20, Grid 9 20, Grid 11 20, Grid 14 20, Grid 15 20, Grid 20 20, Grid 21 20, Grid 23 20, Grid 24 20, Grid 25 20, Grid 26 20, Grid 28 20, Grid 29 20, Grid 0 21, Grid 1 21, Grid 3 21, Grid 5 21, Grid 6 21, Grid 8 21, Grid 9 21, Grid 10 21, Grid 11 21, Grid 12 21, Grid 13 21, Grid 14 21, Grid 17 21, Grid 18 21, Grid 20 21, Grid 21 21, Grid 22 21, Grid 23 21, Grid 24 21, Grid 25 21, Grid 26 21, Grid 27 21, Grid 29 21, Grid 0 22, Grid 1 22, Grid 2 22, Grid 3 22, Grid 4 22, Grid 5 22, Grid 6 22, Grid 8 22, Grid 9 22, Grid 10 22, Grid 11 22, Grid 12 22, Grid 13 22, Grid 14 22, Grid 15 22, Grid 16 22, Grid 17 22, Grid 19 22, Grid 20 22, Grid 21 22, Grid 22 22, Grid 23 22, Grid 24 22, Grid 26 22, Grid 27 22, Grid 28 22, Grid 29 22, Grid 0 23, Grid 1 23, Grid 2 23, Grid 3 23, Grid 4 23, Grid 5 23, Grid 6 23, Grid 7 23, Grid 9 23, Grid 11 23, Grid 12 23, Grid 14 23, Grid 16 23, Grid 18 23, Grid 19 23, Grid 21 23, Grid 22 23, Grid 23 23, Grid 24 23, Grid 25 23, Grid 26 23, Grid 27 23, Grid 28 23, Grid 29 23]]
      , ..
    },


    -- Tunnel Map
    Problem {
        problemWidth = 15
      , problemHeight = 15
      , problemShapes = [ unitShape ]
      , problemSeeds = seedNum 1
      , problemLen   = 100
      , problemFilled = [Grid x (y+6) | Grid x y <- [Grid 0 2, Grid 1 2, Grid 2 2, Grid 6 2, Grid 7 2, Grid 8 2, Grid 9 2, Grid 10 2, Grid 11 2, Grid 12 2, Grid 13 2, Grid 14 2, Grid 0 3, Grid 1 3, Grid 2 3, Grid 3 3, Grid 7 3, Grid 8 3, Grid 9 3, Grid 10 3, Grid 11 3, Grid 12 3, Grid 13 3, Grid 14 3, Grid 0 4, Grid 1 4, Grid 2 4, Grid 3 4, Grid 4 4, Grid 5 4, Grid 9 4, Grid 10 4, Grid 11 4, Grid 12 4, Grid 13 4, Grid 14 4, Grid 0 5, Grid 1 5, Grid 2 5, Grid 3 5, Grid 7 5, Grid 8 5, Grid 9 5, Grid 10 5, Grid 11 5, Grid 12 5, Grid 13 5, Grid 14 5, Grid 0 6, Grid 1 6, Grid 2 6, Grid 3 6, Grid 4 6, Grid 8 6, Grid 9 6, Grid 10 6, Grid 11 6, Grid 12 6, Grid 13 6, Grid 14 6, Grid 0 7, Grid 1 7, Grid 2 7, Grid 6 7, Grid 7 7, Grid 8 7, Grid 9 7, Grid 10 7, Grid 11 7, Grid 12 7, Grid 13 7, Grid 14 7, Grid 0 8, Grid 1 8, Grid 2 8, Grid 6 8, Grid 7 8, Grid 8 8, Grid 9 8, Grid 10 8, Grid 11 8, Grid 12 8, Grid 13 8, Grid 14 8]]
      , ..
    }


  , Problem
      { problemWidth  = 15
      , problemHeight = 15
      , problemShapes = map (shiftCenter 0 5)
                        [ horizontalBarbell,
                          verticalBarbell, twoOclockTriangle,
                          threeOclockTriange
                        ]
      , problemFilled = []
      , problemLen    = 100
      , problemSeeds  = seedNum 1
      , ..
      }    
  , Problem
      { problemWidth  = 10
      , problemHeight = 8
      , problemShapes = [ unitShape ]
      , problemFilled =
          [ f | r <- take 10 [ 2, 4 .. ]
              , c <- take 4 [ 1, 3 .. ]
              , f@(Grid x y) <- [ Grid c r, Grid (c + 1) r
                     , Grid (c+2) (r + 1), Grid (c + 3) (r + 1) ]
              , x < 10, y < 8
          ]
      , problemLen    = 20
      , problemSeeds  = seedNum 1
      , ..
      }

  , Problem
      { problemWidth  = 10
      , problemHeight = 8
      , problemShapes = [ GridShape { center = Grid 0 0
                        , members = [Grid 0 0, Grid 1 0] } ]
      , problemFilled =
          [ f | r <- take 10 [ 2, 4 .. ]
              , c <- take 4 [ 1, 3 .. ]
              , f@(Grid x y) <- [ Grid c r, Grid (c + 1) r
                     , Grid (c+2) (r + 1), Grid (c + 3) (r + 1) ]
              , x < 10, y < 8
          ]
      , problemLen    = 100
      , problemSeeds  = seedNum 1
      , ..
      }

  , Problem
      { problemWidth  = 10
      , problemHeight = 9
      , problemShapes = [ GridShape { center = Grid 1 0
                        , members = [Grid 0 0, Grid 1 0, Grid 2 0] } ]
      , problemFilled =
          [ Grid x (y + 1) | r <- take 10 [ 2, 4 .. ]
              , c <- take 4 [ 1, 3 .. ]
              , Grid x y <- [ Grid c r, Grid (c + 1) r ]
              , x < 10, y < 8
          ]
      , problemLen    = 100
      , problemSeeds  = seedNum 1
      , ..
      }

  , Problem
    { problemWidth = 100
    , problemHeight = 40
    , problemShapes =
        let singlePiece = GridShape { center = Grid 4 0, members = [ Grid 0 0 ] }
            doublePiece = GridShape { center = Grid 4 0, members = [ Grid 0 0, Grid 1 0 ] }
            triplePiece = GridShape { center = Grid 4 0, members = [ Grid 0 0, Grid 1 0, Grid 2 0 ] }
        in foldr (uncurry replace) (replicate 27 singlePiece ++ replicate 18 doublePiece ++ replicate 9 triplePiece)
        [ ( 0, GridShape { center = Grid 2 2, members = [Grid 0 0, Grid 1 0, Grid 2 0, Grid 3 0, Grid 1 1, Grid 1 2, Grid 1 3, Grid 0 4, Grid 1 4, Grid 2 4, Grid 3 4]})
        , (23, GridShape { center = Grid 2 2, members = [Grid 1 0, Grid 2 0, Grid 3 0, Grid 0 1, Grid 0 2, Grid 0 3, Grid 1 4, Grid 2 4, Grid 3 4]})
        , (32, GridShape { center = Grid 1 3, members = [Grid 0 0, Grid 1 0, Grid 2 0, Grid 0 1, Grid 0 2, Grid 1 2, Grid 2 2, Grid 0 3, Grid 0 4]})
        , (47, GridShape { center = Grid 1 2, members = [Grid 0 0, Grid 1 0, Grid 2 0, Grid 0 1, Grid 2 1, Grid 0 2, Grid 1 2, Grid 2 2, Grid 0 3, Grid 0 4]})
        , (20, GridShape { center = Grid 2 2, members = [Grid 0 0, Grid 1 0, Grid 2 0, Grid 2 1, Grid 1 2, Grid 2 2, Grid 0 3, Grid 0 4, Grid 1 4, Grid 2 4, Grid 3 4]})
        , (53, GridShape { center = Grid 2 2, members = [Grid 1 0, Grid 2 0, Grid 3 0, Grid 0 1, Grid 3 1, Grid 0 2, Grid 4 2, Grid 0 3, Grid 3 3, Grid 1 4, Grid 2 4, Grid 3 4, Grid 2 2]})
        , (43, GridShape { center = Grid 2 2, members = [Grid 2 0, Grid 3 0, Grid 2 1, Grid 2 2, Grid 1 3, Grid 0 4, Grid 1 4, Grid 2 4, Grid 3 4]})
        , (35, GridShape { center = Grid 2 2, members = [Grid 0 0, Grid 1 0, Grid 2 0, Grid 3 0, Grid 0 1, Grid 1 2, Grid 2 2, Grid 3 2, Grid 3 3, Grid 0 4, Grid 1 4, Grid 2 4, Grid 3 4]})
        ]
    , problemFilled = couv
    , problemLen    = 1620
    , problemSeeds  = [18]
    , ..
    }


  ]
  where
  tet3 = [GridShape {center = Grid 0 1, members = [Grid 1 0,Grid 0 1,Grid 0 2]},GridShape {center = Grid 1 0, members = [Grid 2 0,Grid 1 0,Grid 0 1]},GridShape {center = Grid 0 0, members = [Grid 1 1,Grid 1 0,Grid 0 1]}]
  tet4 = [GridShape {center = Grid 1 1, members = [Grid 2 0,Grid 1 1,Grid 1 2,Grid 0 3]},GridShape {center = Grid 1 1, members = [Grid 2 0,Grid 1 0,Grid 0 1,Grid 0 2]},GridShape {center = Grid 0 1, members = [Grid 1 1,Grid 1 0,Grid 0 1,Grid 0 2]},GridShape {center = Grid 0 1, members = [Grid 0 0,Grid 1 0,Grid 0 1,Grid 0 2]},GridShape {center = Grid 0 1, members = [Grid 1 0,Grid 1 1,Grid 1 2,Grid 0 3]},GridShape {center = Grid 1 1, members = [Grid 2 0,Grid 1 1,Grid 0 1,Grid 0 2]},GridShape {center = Grid 1 0, members = [Grid 2 1,Grid 2 0,Grid 1 0,Grid 0 1]},GridShape {center = Grid 1 0, members = [Grid 1 1,Grid 2 0,Grid 1 0,Grid 0 1]},GridShape {center = Grid 0 1, members = [Grid 0 0,Grid 0 1,Grid 1 1,Grid 0 2]},GridShape {center = Grid 1 0, members = [Grid 0 1,Grid 1 1,Grid 3 0,Grid 2 0]}]


moveBy x y (Grid a b) = Grid (a + x) (b + y)


couv = map (moveBy 0 10) [Grid 14 0, Grid 13 1, Grid 14 2, Grid 13 3, Grid 14 4, Grid 13 5, Grid 14 6, Grid 44 6, Grid 45 6, Grid 46 6, Grid 12 7, Grid 13 7, Grid 14 7, Grid 15 7, Grid 42 7, Grid 43 7, Grid 46 7, Grid 47 7, Grid 11 8, Grid 12 8, Grid 16 8, Grid 17 8, Grid 41 8, Grid 42 8, Grid 48 8, Grid 49 8, Grid 50 8, Grid 10 9, Grid 17 9, Grid 41 9, Grid 49 9, Grid 11 10, Grid 12 10, Grid 16 10, Grid 17 10, Grid 41 10, Grid 49 10, Grid 12 11, Grid 15 11, Grid 41 11, Grid 49 11, Grid 13 12, Grid 15 12, Grid 41 12, Grid 49 12, Grid 77 12, Grid 78 12, Grid 79 12, Grid 80 12, Grid 81 12, Grid 82 12, Grid 98 12, Grid 99 12, Grid 9 13, Grid 10 13, Grid 11 13, Grid 12 13, Grid 15 13, Grid 16 13, Grid 17 13, Grid 30 13, Grid 31 13, Grid 32 13, Grid 33 13, Grid 34 13, Grid 35 13, Grid 41 13, Grid 49 13, Grid 76 13, Grid 82 13, Grid 98 13, Grid 10 14, Grid 17 14, Grid 31 14, Grid 35 14, Grid 41 14, Grid 49 14, Grid 73 14, Grid 74 14, Grid 75 14, Grid 77 14, Grid 82 14, Grid 98 14, Grid 9 15, Grid 17 15, Grid 30 15, Grid 35 15, Grid 41 15, Grid 49 15, Grid 51 15, Grid 52 15, Grid 53 15, Grid 69 15, Grid 72 15, Grid 75 15, Grid 76 15, Grid 82 15, Grid 98 15, Grid 10 16, Grid 17 16, Grid 31 16, Grid 35 16, Grid 41 16, Grid 50 16, Grid 51 16, Grid 54 16, Grid 55 16, Grid 56 16, Grid 69 16, Grid 70 16, Grid 72 16, Grid 82 16, Grid 83 16, Grid 98 16, Grid 0 17, Grid 1 17, Grid 2 17, Grid 3 17, Grid 9 17, Grid 17 17, Grid 30 17, Grid 35 17, Grid 41 17, Grid 55 17, Grid 67 17, Grid 68 17, Grid 70 17, Grid 71 17, Grid 83 17, Grid 89 17, Grid 90 17, Grid 91 17, Grid 98 17, Grid 4 18, Grid 10 18, Grid 17 18, Grid 18 18, Grid 19 18, Grid 20 18, Grid 31 18, Grid 35 18, Grid 41 18, Grid 56 18, Grid 57 18, Grid 68 18, Grid 83 18, Grid 87 18, Grid 88 18, Grid 89 18, Grid 92 18, Grid 93 18, Grid 94 18, Grid 95 18, Grid 98 18, Grid 3 19, Grid 9 19, Grid 20 19, Grid 30 19, Grid 35 19, Grid 41 19, Grid 56 19, Grid 67 19, Grid 83 19, Grid 84 19, Grid 85 19, Grid 86 19, Grid 95 19, Grid 96 19, Grid 98 19, Grid 4 20, Grid 10 20, Grid 20 20, Grid 31 20, Grid 35 20, Grid 41 20, Grid 57 20, Grid 68 20, Grid 97 20, Grid 98 20, Grid 3 21, Grid 9 21, Grid 20 21, Grid 30 21, Grid 35 21, Grid 41 21, Grid 56 21, Grid 65 21, Grid 66 21, Grid 67 21, Grid 4 22, Grid 10 22, Grid 20 22, Grid 24 22, Grid 25 22, Grid 26 22, Grid 31 22, Grid 36 22, Grid 37 22, Grid 41 22, Grid 57 22, Grid 63 22, Grid 64 22, Grid 65 22, Grid 4 23, Grid 7 23, Grid 8 23, Grid 9 23, Grid 20 23, Grid 22 23, Grid 23 23, Grid 24 23, Grid 25 23, Grid 26 23, Grid 27 23, Grid 30 23, Grid 37 23, Grid 38 23, Grid 39 23, Grid 41 23, Grid 56 23, Grid 63 23, Grid 4 24, Grid 7 24, Grid 20 24, Grid 22 24, Grid 28 24, Grid 31 24, Grid 39 24, Grid 41 24, Grid 57 24, Grid 63 24, Grid 4 25, Grid 6 25, Grid 20 25, Grid 22 25, Grid 28 25, Grid 29 25, Grid 30 25, Grid 39 25, Grid 40 25, Grid 56 25, Grid 58 25, Grid 59 25, Grid 60 25, Grid 61 25, Grid 62 25, Grid 5 26, Grid 6 26, Grid 7 26, Grid 20 26, Grid 22 26, Grid 57 26, Grid 59 26, Grid 20 27, Grid 22 27, Grid 57 27, Grid 58 27, Grid 20 28, Grid 22 28, Grid 20 29, Grid 21 29, Grid 22 29]

replace :: Int -> a -> [a] -> [a]
replace n x xs = take n xs ++ [x] ++ drop (n+1) xs
