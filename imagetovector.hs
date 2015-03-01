import Data.Packed.Vector
--import qualified Data.Vector as FV
import Vision.Image
--import Vision.Image.Storage.DevIL (Autodetect (..), load)

imageToVector :: Manifest RGBPixel -> Vector Double
imageToVector = fromList . concatMap f . toList . manifestVector
  where
  f :: RGBPixel -> [Double]
  f (RGBPixel r g b) = [fromIntegral r, fromIntegral g, fromIntegral b]