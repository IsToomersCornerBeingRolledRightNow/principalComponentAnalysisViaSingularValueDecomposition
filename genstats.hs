-- we're passed a directory path .../chopped/someNumber
-- this dir contains 10k images and a hyperplane.txt
-- we need to compare each image to the hyperplane, take the mean and std dev, and save that in a text file
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

