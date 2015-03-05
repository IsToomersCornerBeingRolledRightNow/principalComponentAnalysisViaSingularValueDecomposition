--module Train where
{-# LANGUAGE BangPatterns #-}
import PCA
import ImageToVector

import System.Directory
import System.Environment
import Data.Packed.Matrix
import Data.Packed.Vector
import Numeric.LinearAlgebra.HMatrix

main :: IO ()
main = do
  [dir, width, height, threshold] <- getArgs
  let h' = (read height) :: Int
  let w' = (read width) :: Int
  let t' = (read threshold) :: Int
  imageFiles <- getDirectoryContents dir 
  let imagePaths = map ((dir ++ "/") ++) imageFiles
  maybeImages <- loadImages w' h' imagePaths
  let r = [a | Just a <- maybeImages]
  mapM_ print r
  let !matrix = fromRows r
  print matrix
  print $ (rows matrix, cols matrix)
  --let (s,(Hyperplane dim mean rows)) = linRegressionDebug t' matrix
  --print dim
  --print s
  print $ rightSV matrix
 