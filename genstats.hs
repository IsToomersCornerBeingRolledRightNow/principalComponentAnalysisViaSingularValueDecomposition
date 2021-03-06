-- genstats.hs
-- Compiles to executable.
-- We're passed a directory path .../chopped/someNumber containing
-- training data (presumably 'typical' images from the camera feed) and
-- the hyperplane.txt generated by train.hs.
-- We compare each image to the hyperplane, take the mean distance,
-- and save that in a text file in the same directory.
-- This completes analysis of the training data.
import PCA (loadHyperplane, distance, Hyperplane)
import System.Environment (getArgs)
import ImageToVector 
  ( loadImagesToVectors
  , imageToVector
  , chop
  , loadImage
  , loadImages
  , changeResolution)
import Data.Maybe (catMaybes)
import System.Directory (getDirectoryContents)
import System.IO (withFile, hPrint, IOMode (..))

main = do
  dir:_ <- getArgs
  let planefile = dir ++ "/hyperplane.txt"
      sigmafile = dir ++ "/avgdist.txt"
  hypl <- loadHyperplane planefile
  let f = loadImagesToVectors 20 20 . fmap ((dir++)"/"++)
  imgs <- fmap catMaybes $ f =<< getDirectoryContents dir
  let ds = fmap (distance hypl) imgs
  let sigma = avg ds
  print sigma
  withFile sigmafile WriteMode (flip hPrint sigma)


avg xs = sum xs / (fromIntegral $ length xs)

