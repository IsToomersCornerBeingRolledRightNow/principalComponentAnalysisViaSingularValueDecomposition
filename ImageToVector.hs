module ImageToVector (loadImages) where
import Data.Packed.Vector
import Vision.Image
import Vision.Primitive.Shape
import Vision.Image.Storage.DevIL (BMP (..), load)

imageToVector :: RGB -> Vector Double
imageToVector = fromList . concatMap f . toList . manifestVector
  where
  f :: RGBPixel -> [Double]
  f (RGBPixel r g b) = [fromIntegral r, fromIntegral g, fromIntegral b]
  
changeResolution :: Int -> Int -> RGB -> RGB
changeResolution w h img = resize NearestNeighbor (Z :. w :. h) img

loadImage :: Int -> Int -> FilePath -> IO (Vector Double)
loadImage w h path = do
  img <- load BMP path
  case img of
       Left err -> do
         putStrLn "Error loading image:"
         print err
         undefined
       Right rgb -> do
         return $ imageToVector $ changeResolution w h rgb
         
loadImages :: [FilePath] -> Int -> Int -> IO [Vector Double]
loadImages paths w h = mapM (loadImage h w) paths
