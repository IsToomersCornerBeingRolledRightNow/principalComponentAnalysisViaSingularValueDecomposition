module ImageToVector (loadImage, loadImages) where
-- we will end up needing the loadImage function
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

loadImage :: Int -> Int -> FilePath -> IO (Maybe (Vector Double))
loadImage w h path = do
  img <- load BMP path
  case img of
       Left err -> do
         putStrLn "Error loading image:"
         print err
         return Nothing
       Right rgb -> do
         return $ Just (imageToVector $ changeResolution w h rgb)
         
loadImages :: Int -> Int -> [FilePath] -> IO [Maybe (Vector Double)]
-- argument order changed to agree with loadImage
loadImages w h = mapM (loadImage h w) -- point-free style, also we can more clearly see the action of mapM this way
