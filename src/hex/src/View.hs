{-# LANGUAGE RecordWildCards #-}

import Hex
import Problem
import Grid
import Power

import ShapeSets

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort(ViewPort)
import qualified Data.Vector as Vector
import  Data.Map ( Map )
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Maybe(mapMaybe)
import System.Environment(getArgs)
import System.Exit(exitFailure)
import System.IO(stderr,hPutStrLn)
import Text.Read(readMaybe)
import qualified Data.Aeson as JS
import qualified Data.ByteString.Lazy as LBS

winW, winH :: Int
winW = 1300
winH = 700

radius, toSide, spacing :: Float
radius  = 10
toSide  = radius * (pi / 3)
spacing = radius / 5

boardFull, boardEmpty, shapeColor, centerColor :: Color
txtColor    = white
bgColor     = black
boardFull   = white
boardEmpty  = greyN 0.3
shapeColor  = red
centerColor = white
powerColor       = black
activePowerColor = red

gridToScreen :: Grid -> Point
gridToScreen (Grid x y) = ( ch + fromIntegral x * (2 * toSide + spacing)
                          , fromIntegral y * (1.5 * radius + spacing)
                          )
  where ch = if y `mod` 2 == 0 then 0 else toSide

drawUnit :: Picture
drawUnit = polygon [ (w*px,h*py) | (px,py) <- sh ]
  where
  w  = 2 * toSide
  h  = 2 * radius
  sh = [ (0.5, 0), (1,0.25), (1,0.75), (0.5, 1), (0,0.75), (0,0.25) ]

drawCenter :: Picture
drawCenter = color centerColor
           $ translate toSide radius
           $ circleSolid (radius / 3)

drawGridShape :: GridShape -> Picture
drawGridShape GridShape { .. } =
  color shapeColor $
  pictures (map (`at` drawUnit) members ++ [ at center drawCenter ])
  where

at :: Grid -> Picture -> Picture
at = uncurry translate . gridToScreen

drawBoard :: Board -> Picture
drawBoard Board { .. } =
  pictures [ color (if full then boardFull else boardEmpty)
                     (at (Grid x y) drawUnit)
                | x <- [ 0 .. boardWidth - 1 ]
                , y <- [ 0 .. boardHeight - 1 ]
                , let full = boardLayout Vector.! y Vector.! x
                ]

drawStep :: Step -> Picture
drawStep Step { .. } = pictures [ drawBoard board, drawGridShape shape ]


drawPower (as,b:bs) =
  Just ( pictures [ color powerColor (drawText as)
                  , at (Grid n 0) $ color activePowerColor (drawLetter b)
                  , at (Grid (n+1) 0)
                    $ color powerColor (drawText bs)
                  ]
        , (as ++ [b], bs))
  where
  n = fromIntegral (length as)

drawPower _ = Nothing


drawText :: String -> Picture
drawText xs = pictures [ at (Grid n 0) (drawLetter c)
                                      | (n,c) <- zip [ 0 .. ] xs ]

drawLetter :: Char -> Picture
drawLetter c = pictures [ drawUnit
                        , color txtColor
                          $ translate (toSide /2) (radius/2)
                          $ scale 0.1 0.1 (text [c])
                        ]



data S = S
  { activePowers :: [ (String,String) ]
  , pic :: Picture
  , frames :: [Frame]
  , powerMap :: Map Text Integer
  }

fitToWin :: Int -> Int -> Picture -> Picture
fitToWin w h p =
               translate (-dx) ((-dy -y*sc))
               $ scale sc sc p
  where
  w' = fromIntegral winW
  h' = fromIntegral winH
  dx = w' / 2
  dy = h' / 2
  sc = min sx sy
  sx = w' / (fromIntegral (w+1) * (2 * toSide + spacing))
  sy = h' / (fromIntegral (h+extra) * (1.7 * radius + spacing))
  (_,y) = gridToScreen (Grid 0 (-extra))

  extra = 7

nextS :: ViewPort -> Float -> S -> S
nextS _ f S { .. } =
  case frames of
    [] ->  S { .. }
    f : fs ->
      case f of
        FPower txt -> S { activePowers = ("",Text.unpack txt) : activePowers
                        , powerMap = Map.insertWith (+) txt 1 powerMap
                        , frames = fs, .. }
        FStep i st ->
          let (as,bs) = unzip $ mapMaybe drawPower activePowers
              score = show i ++ " / " ++ show (i + scorePowerMap powerMap)
              bd = board st
              b = fitToWin (boardWidth bd) (boardHeight bd)
                $ pictures (  drawStep st
                           :  at (Grid 0 (-5)) (scale 0.2 (-0.2)
                                $ color txtColor $ text $ score)
                           :  [ at (Grid 0 (negate n)) (scale 1 (-1) a)
                                          | (n,a) <- zip [ 1 .. ] as ] )
          in S { activePowers = bs, frames = fs, pic = scale 1 (-1) b, .. }




data Frame = FStep Integer Step | FPower Text
               deriving Show




main :: IO ()
main =
  do args <- getArgs
     (win,fs,speed) <-
        case args of
          powerWordFile : speedTxt : more
            | Just speed <- readMaybe speedTxt ->
            do allPws <- getPowerWords powerWordFile
               (pid,seed,cs) <-
                  do bs <- LBS.getContents
                     case JS.decode bs of
                       Just (sln : _) ->
                         return ( solutionProblemId sln
                                , fromIntegral (solutionProblemSeed sln)
                                , solutionCmds sln)
                       _ -> reportError "Failed to decode solution"

               mbP <- loadProblem pid
               p <- case mbP of
                      Nothing -> reportError "No such problem"
                      Just p -> return p
               let win = InWindow
                           ("Problem " ++ show pid ++ ", seed " ++ show seed)
                           (winW, winH)
                           (10,10)
                   st = gameSteps (gameStates seed p (decodeText cs))
                   pws = findPower allPws cs
                   fs = weave st pws

                   fs1 = case more of
                           x : _ | Just a <- readMaybe x -> drop a fs
                           _ -> fs
                   fs2 = case more of
                           _ : x : _ | Just a <- readMaybe x -> take a fs1
                           _ -> fs1
               return (win,fs2,speed)

          _ -> reportError "Usage: POWER_WORD_FILE FPS [SKIP] [TAKE] < SOLUTION"

     let s0 = S { activePowers = [], pic = blank, frames = fs, powerMap = Map.empty }

     simulate win bgColor speed s0 pic nextS

reportError :: String -> IO a
reportError e = do hPutStrLn stderr e
                   exitFailure




-- For video replay. weaves game steps and power phrases.
weave :: [(Integer,Step)] -> [(Integer,Text)] -> [Frame]
weave ss0 ps0 = go 0 ss0 ps0
  where
  go n ss ((x,t) : more) | n == x   = FPower t : go n ss more
  go _ [] _ = []
  go n ((i,s) : ss) ps = FStep i s : go (n+1) ss ps



