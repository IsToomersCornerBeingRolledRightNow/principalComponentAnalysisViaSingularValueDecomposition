-- train.hs
-- Compiles to executable.
-- We're passed (1) the path to a directory and (2) an integer.
-- Saves a hyperplane (dim = 2nd arg) that is the linear regression
-- of all of the images in the directory to hyperplane.txt in the same
-- directory.
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
    , " \tsv: ", show numsv,", \t"]
  hFlush stdout
  imageFiles <- getDirectoryContents dir
  maybeImages <- loadImagesToVectors 20 20 
                 $ map ((dir ++ "/") ++) imageFiles
  let rows = takeImgs [a | Just a <- maybeImages]
  putStr $ concat 
    [ show . length $ rows, " images. "]
  hFlush stdout
  let h = linRegression numsv $ fromRows rows
  saveHyperplane (dir++"/hyperplane.txt") h
  putStrLn "Done!"

