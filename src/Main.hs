module Main where

import Image
import Codec.Picture
import System.Environment
import Types
import Wave
import System.Random
import Data.Either
import Control.Lens

  -- it doesn't like nonsquare input
main :: IO ()
main = do
  [imgPath, outHeight, outWidth] <- getArgs
  decoder <- readImage $ imgPath
  (gen1, gen2) <- getStdGen >>= pure . split
  let dims = (read outHeight, read outWidth) 

  let img = convertRGB8 $ fromRight (error "invalid image format") decoder  
      collapsed = uncurry (observe dims gen1) $ parseImage gen2 img dims 
      out = mkImg dims $  collapsed 
  writePng  "output.png"   out 
