import Data.Packed.Vector
--import qualified Data.Vector as FV
import Vision.Image
--import Vision.Image.Storage.DevIL (Autodetect (..), load)

imageToVector :: RGB -> Vector Double
imageToVector = fromList . concatMap f . toList . manifestVector
  where
  f :: RGBPixel -> [Double]
  f (RGBPixel r g b) = [fromIntegral r, fromIntegral g, fromIntegral b]
  
reduceResolution :: Int -> Int -> RGB -> RGB
reduceResolution h w img = resize NearestNeighbor (manifestSize img) img