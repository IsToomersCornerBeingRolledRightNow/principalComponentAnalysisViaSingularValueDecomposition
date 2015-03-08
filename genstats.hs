-- we're passed a directory path .../chopped/someNumber
-- this dir contains 10k images and a hyperplane.txt
-- we need to compare each image to the hyperplane, take the mean and std dev, and save that in a text file
module Genstats where

import PCA (loadHyperplane, distance, Hyperplane)
import System.Environment (getArgs)
import ImageToVector 
  ( loadImageToVector
  , imageToVector
  , chop
  , loadImage
  , loadImages
  , changeResolution)
import Data.Maybe (catMaybes)
import System.Directory (getDirectoryContents)

main = do
  [dir,_] <- getArgs
  let imgs = fmap catMaybes $ maybeImgs
        where
        maybeImgs = loadImages =<< getDirectoryContents dir
      hypl = loadHyperplane $ dir ++ "hyperplane.txt"
  undefined
  
