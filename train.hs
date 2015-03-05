import PCA
import ImageToVector

import System.Directory
import System.Environment
import Data.Packed.Matrix
import Data.Packed.Vector

main :: IO ()
main = do
  [dir, width, height, threshold] <- getArgs
  let h' = read height
  let w' = read width
  let t' = read threshold
  imageFiles <- getDirectoryContents dir
  let imagePaths = map (("./" ++ dir) ++) imageFiles
  maybeImages <- loadImages w' h' imagePaths
  let rows = [a | Just a <- maybeImages]
  let matrix = fromRows rows
  let linreg = linRegression t' matrix
  print linreg

