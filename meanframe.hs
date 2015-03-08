import ImageToVector (loadImagesToVectors, vectorToImage)
import System.Environment (getArgs)
import Data.Maybe (catMaybes)
import System.Directory (getDirectoryContents)
import Data.Packed.Vector
import Numeric.LinearAlgebra.HMatrix (scale)
import Vision.Primitive.Shape (ix2)
import Vision.Image.Storage.DevIL

main = do
  dir:_ <- getArgs
  let f = loadImagesToVectors 1280 720 . fmap ((dir++)"/"++)
  imgs <- fmap catMaybes $ f =<< getDirectoryContents dir
  let avg = scale (1 / (fromIntegral $ length imgs)) (sum imgs)
  let img = vectorToImage (ix2 1280 720) avg
  save PNG (dir ++ "/average/average.png") img

