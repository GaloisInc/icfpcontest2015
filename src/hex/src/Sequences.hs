{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Sequences where

import Data.Word(Word16,Word32,Word64)
import Data.Bits(shiftR, (.&.))
import Data.List
import qualified Data.Vector as Vector

generate :: Word32 -> Int -> [a] -> [a]
generate s len gen
  | sz == 0   = []
  | otherwise = take len [ a Vector.! (i `mod` sz) | i <- randomNumbers s ]
  where
  a  = Vector.fromList gen
  sz = Vector.length a

-- | An LCG generator, with the constants as recommended in C99/C11 ICO/IEC9889
-- (see https://en.wikipedia.org/wiki/Linear_congruential_generator)
step :: Word32 -> (Word16,Word32)
step s = (fromIntegral (s `shiftR` 16) .&. 0x7FFF, s1)
  where
  s1 = fromIntegral (fromIntegral s * a + c)
  a  = 1103515245 :: Word64
  c  = 12345

-- A list of random number using the given seed.
randomNumbers :: Word32 -> [Int]
randomNumbers = map fromIntegral . unfoldr (Just . step)

