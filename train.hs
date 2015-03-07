import PCA
import ImageToVector

import System.Directory
import System.IO (hFlush, stdout)
import System.Environment
import Data.Packed.Matrix
import Data.Packed.Vector

main :: IO ()
main = do
  (arg1:arg2:rest) <- getArgs
  let dir = arg1
      numsv = read arg2
      takeImgs = case rest of
                []     -> id :: [a] -> [a]
                arg3:_ -> take (read arg3)
  putStr $ concat 
    [ "Dir: ", dir
    , "sv: ", show numsv,", "]
  hFlush stdout
  imageFiles <- getDirectoryContents dir
  let imagePaths = takeImgs $ map ((dir ++ "/") ++) imageFiles
  maybeImages <- loadImagesToVectors 80 80 imagePaths
  let rows = [a | Just a <- maybeImages]
  putStr $ concat 
    [ show.length$rows, " images. "]
  hFlush stdout
  let matrix = fromRows rows
  let h@(Hyperplane dim mean rows) = linRegression numsv matrix
  saveHyperplane (dir++"/hyperplane.txt") h
  putStrLn $ concat
    [ "Done! ", show dim]

