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
  [dir] <- getArgs
  imageFiles <- getDirectoryContents dir 
  let imagePaths = map ((dir ++ "/") ++) imageFiles
  maybeImages <- loadImages imagePaths
  let images = [a | Just a <- maybeImages]
  let pieces = map (chop 80 80) images
  print $ map length pieces