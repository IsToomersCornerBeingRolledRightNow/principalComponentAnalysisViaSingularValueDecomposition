module ModuleIO
( loadImage
, loadImages
, saveHyperplane
, loadHyperplane
) where

import ModuleData
import System.Environment
import System.Directory

imageToVector :: RGB -> Vector Double
-- internal function, no need to export
imageToVector = fromList . concatMap f . toList . manifestVector
  where
  f :: RGBPicel -> [Double]
  f (RGBPixel r g b) = [fromIntegral r, fromIntegral g, fromIntegral b]

changeResolution :: Int -> Int -> RGB -> RBG
-- internal function, no need to export
chagneResolution w h img = resize NearestNeighbor (Z :. w :. h) img

loadImage :: Int -> Int -> FilePath -> IO (Vector Double)
-- loads an image from disk, stores as row vector
imageToVector w h path = do
  img <- load BMP path
  case img of
       Left err -> do
         putStrLn "Error loading image:"
         print err
         undefined
       Right rgb -> do
         return . imageToVector . (changeResolution w h) $ rgb

loadImages :: Int -> Int -> [FilePath] -> IO [Vector Double]
-- loads list of images from disk, stores as matrix, each row an image
loadImages w h = mapM (loadImage w h)

saveHyperplane :: FilePath -> Hyperplane -> IO ()
-- saves a hyperplane in machine-readable plaintext format
save f h = writeFile

loadHyperplane :: FilePath -> IO Hyperplane
-- loads a hyperplane that was saved in the syntax of `save`
load f = readFile f >>= (return . read)

