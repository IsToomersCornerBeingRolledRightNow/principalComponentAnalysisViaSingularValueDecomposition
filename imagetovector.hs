{-# LANGUAGE ScopedTypeVariables #-}
module ImageToVector (loadImages) where
import Data.Packed.Vector
--import qualified Data.Vector as FV
import Vision.Image
import Vision.Image.Storage.DevIL (BMP (..), load)

imageToVector :: RGB -> Vector Double
imageToVector = fromList . concatMap f . toList . manifestVector
  where
  f :: RGBPixel -> [Double]
  f (RGBPixel r g b) = [fromIntegral r, fromIntegral g, fromIntegral b]
  
changeResolution :: Int -> Int -> RGB -> RGB
changeResolution h w img = resize NearestNeighbor (manifestSize img) img

loadImage :: Int -> Int -> FilePath -> IO (Vector Double)
loadImage h w path = do
  img <- load BMP path
  case img of
       Left err -> do
         putStrLn "Error loading image:"
         print err
         undefined
       Right (rgb :: RGB) -> do
         return $ imageToVector $ changeResolution h w rgb
         
loadImages :: [FilePath] -> Int -> Int -> IO [Vector Double]
loadImages paths h w = mapM (loadImage h w) paths