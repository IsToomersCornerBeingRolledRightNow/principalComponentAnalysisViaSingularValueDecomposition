import PCA (loadHyperplane, distance, Hyperplane)
import System.Environment (getArgs)
import ImageToVector 
  ( loadImageToVector
  , imageToVector
  , chop
  , loadImage 
  , changeResolution)
import Data.Maybe (fromJust)

main = do
  [hyperplaneFolder, imgPath,_] <- getArgs
  loadImage imgPath 
     >>= fromJust
     # chop 80 80 
     # fmap (changeResolution 20 20)
     # zipWith (f hyperplaneFolder) [1..] 
     # sequence
     >>= fmap (^2)
     # sum
     # return
  where
  f hdir n img = do
    h <- loadHyperplane . concat
         $ [hdir,"/",show n,"/hyperplane.txt"]
    return $ distance h (imageToVector img)
  sumOfSquares = sum . fmap (^2)
infixl 3 #
(#) :: (a -> b) -> (b -> c) -> (a -> c)
(#) = flip (.)

