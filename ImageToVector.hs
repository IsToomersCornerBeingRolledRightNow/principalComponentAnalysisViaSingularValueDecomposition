module ImageToVector (loadImages) where
import Data.Packed.Vector
import Vision.Image
import Vision.Image.Storage.DevIL (Autodetect (..), load)

imageToVector :: RGB -> Vector Double
imageToVector = fromList . concatMap f . toList . manifestVector
  where
  f :: RGBPixel -> [Double]
  f (RGBPixel r g b) = [fromIntegral r, fromIntegral g, fromIntegral b]
  
changeResolution :: Int -> Int -> RGB -> RGB
changeResolution h w img = resize NearestNeighbor (manifestSize img) img

loadImage :: Int -> Int -> FilePath -> IO (Vector Double)
loadImage h w path = do
  img <- load Autodetect path
  case img of
       Left err -> do
         putStrLn "Error loading image:"
         print err
         undefined
       Right rgb -> do
         return $ imageToVector $ changeResolution h w rgb
         
loadImages :: [FilePath] -> Int -> Int -> IO [Vector Double]
loadImages paths h w = mapM (loadImage h w) paths
