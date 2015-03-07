{-# LANGUAGE BangPatterns #-}
import PCA
import ImageToVector

import System.Directory
import System.Environment
import Data.Packed.Matrix
import Data.Packed.Vector
import Numeric.LinearAlgebra.HMatrix
import Vision.Image hiding (map)
import Vision.Image.Storage.DevIL
import Vision.Primitive
import Vision.Primitive.Shape
import qualified Data.Vector.Storable as V

defaultImg :: RGB
defaultImg = Manifest (Z :. 1 :. 1) (V.singleton (RGBPixel 0 0 0))

main :: IO ()
main = do
  [dir] <- getArgs
  imageFiles <- getDirectoryContents dir 
  let imagePaths = map ((dir ++ "/") ++) imageFiles
  maybeImages <- loadImages imagePaths
  let images = [(x,a) | (x,Just a) <- zip imageFiles maybeImages]
  let pieces = (fmap . fmap) (chop 80 80) images
  mapM_ (uncurry $ savePieces dir) pieces
  

  --print $ manifestSize $ (last . head) pieces
  --print $ map length pieces

savePiece :: FilePath -> RGB -> IO ()
savePiece name img = save PNG name img >> return ()

savePieces :: FilePath -> String -> [RGB] -> IO ()
savePieces dir basename pieces = do
  let f n = concat [dir, "/chopped/", show n,"/"]
      paths = fmap f [1..(length pieces)]
      names = fmap (++basename) paths
  mapM_ (createDirectoryIfMissing True) paths
  mapM_ (uncurry $ save PNG) (zip names pieces)

