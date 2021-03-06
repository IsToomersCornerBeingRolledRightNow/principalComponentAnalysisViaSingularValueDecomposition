-- ImageToVector.hs
-- Compiles to a library.
-- This library contains utility functions for loading, converting, and
-- manipulating images.
module ImageToVector
( loadImage
, loadImageToVector
, loadImages
, loadImagesToVectors
, imageToVector
, chop
, changeResolution
, vectorToImage
) where

import Data.Packed.Vector
import Vision.Image hiding (map)
import Vision.Primitive
import Vision.Primitive.Shape
import Vision.Image.Storage.DevIL (Autodetect (..), load)
import Data.Word (Word8)

imageToVector :: RGB -> Vector Double
imageToVector = fromList . concatMap f . toList . manifestVector
  where
  f :: RGBPixel -> [Double]
  f (RGBPixel r g b) = [fromIntegral r, fromIntegral g, fromIntegral b]


vectorToImage :: Size -> Vector Double -> RGB
vectorToImage s v = kludgeImg s
  . toList . mapVector (fromIntegral . floor) $ v

kludgeImg :: Size -> [Word8] -> RGB
kludgeImg s xs = Manifest s $ fromList $ take k $ pxls (xs ++ repeat 0)
  where
  k = let (Z :. w :. h) = s in w * h
  pxls (x:y:z:rest) = RGBPixel x y z : pxls rest
  
  
changeResolution :: Int -> Int -> RGB -> RGB
changeResolution w h img = resize NearestNeighbor (Z :. w :. h) img

loadImageToVector :: Int -> Int -> FilePath -> IO (Maybe (Vector Double))
loadImageToVector w h path = do
  img <- load Autodetect path
  case img of
       Left err -> do
         --putStrLn $ "Error loading image:" ++ path
         --print err
         return Nothing
       Right rgb -> do
         return $ Just (imageToVector $ changeResolution w h rgb)
         
loadImagesToVectors :: Int -> Int -> [FilePath] -> IO [Maybe (Vector Double)]
loadImagesToVectors w h = mapM (loadImageToVector w h)
         
loadImage :: FilePath -> IO(Maybe RGB)
loadImage path = do
  img <- load Autodetect path
  case img of
       Left err -> do
         putStrLn $ "Error loading image:" ++ path
         print err
         return Nothing
       Right rgb -> do
         return $ Just rgb
         
loadImages :: [FilePath] -> IO [Maybe RGB]
loadImages = mapM loadImage

imageDim :: RGB -> (Int,Int)
imageDim img = (w,h)
  where (Z :. h :. w) = manifestSize img

rects :: Int -> Int -> RGB -> [Rect]
rects w h img = (Rect 0 0 w h):(next 0 0)
  where
  next x y = if (x + w >= ((fst . imageDim) img)) && (y + h >= ((snd . imageDim) img))
             then []
             else if x + w >= ((fst . imageDim) img)
             then (Rect 0 (y + h) w h):(next 0 (y+h))
             else (Rect (x + w) y w h):(next (x+w) y)
             
--takes an rgb image and chops it into w by h chunks
chop :: Int -> Int -> RGB -> [RGB]
chop w h img = map ((flip crop) img) (rects w h img)

