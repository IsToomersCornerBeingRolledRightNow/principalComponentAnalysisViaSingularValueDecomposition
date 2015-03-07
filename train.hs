import PCA
import ImageToVector

import System.Directory
import System.Environment
import Data.Packed.Matrix
import Data.Packed.Vector

main :: IO ()
main = do
  [dir, width, height, numsv] <- getArgs
  let h' = read height
  let w' = read width
  let n' = read numsv
  putStr $ concat 
    [ "Dir: ", dir
    , ", WxH: ",width,"x",height,", "
    , "sv: ", numsv," ... "]
  imageFiles <- getDirectoryContents dir
  let imagePaths = map ((dir ++ "/") ++) imageFiles
  maybeImages <- loadImagesToVectors w' h' imagePaths
  let rows = [a | Just a <- maybeImages]
  putStr $ concat 
    [ show.length$rows, " images ... "]
  let matrix = fromRows rows
  let h@(Hyperplane dim mean rows) = linRegression n' matrix
  saveHyperplane (dir++"/hyperplane.txt") h
  putStrLn $ concat
    [ "Done! ", show dim]

