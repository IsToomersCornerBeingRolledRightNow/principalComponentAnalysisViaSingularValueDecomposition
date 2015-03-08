-- meanframe.hs
-- Compiles to executable.
-- We are passed a directory. We find any images in that directory,
-- average them, and save the average as average/average.png.
-- This tool is not used in the training or analysis of images.
import ImageToVector (loadImagesToVectors, vectorToImage)
import System.Environment (getArgs)
import Data.Maybe (catMaybes)
import System.Directory (getDirectoryContents)
import Data.Packed.Vector
import Numeric.LinearAlgebra.HMatrix (scale)
import Vision.Primitive.Shape (ix2)
import Vision.Image.Storage.DevIL
import Data.List (foldl')

main = do
  dir:_ <- getArgs
  let f = loadImagesToVectors 1280 720 . fmap ((dir++)"/"++)
  imgs <- fmap catMaybes $ f =<< getDirectoryContents dir
  let avgimg = vectorToImage (ix2 1280 720) $ avg imgs
  save PNG (dir ++ "/average/average.png") avgimg

avg :: [Vector Double] -> Vector Double
avg xs = scale (1 / fromIntegral num) sum
  where
  (num, sum) = foldl' f zero xs
  zero = (0, fromList . take (1280*720*3) . repeat $ 0)
  f (n,s) v = n `seq` s `seq` (n+1,v+s)

