{-# LANGUAGE BangPatterns #-}
import PCA
import ImageToVector

import System.Directory
import System.Environment
import Data.Packed.Matrix
import Data.Packed.Vector
import Numeric.LinearAlgebra.HMatrix
import Vision.Image hiding (map)
import Vision.Primitive
import Vision.Primitive.Shape
import qualified Data.Vector.Storable as V

defaultImg :: RGB
defaultImg = Manifest (Z :. 1 :. 1) (V.singleton (RGBPixel 0 0 0))

main :: IO ()
main = do
  [imgPath] <- getArgs
  --imageFiles <- getDirectoryContents dir 
  --let imagePaths = map ((dir ++ "/") ++) imageFiles
  maybeImage <- loadImage imgPath
  let img = maybe defaultImg id maybeImage
  let pieces = chop 80 80 img
  print $ length pieces