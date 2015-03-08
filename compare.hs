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
  hyperplanes <- mapM (\k -> 
    loadHyperplane . concat 
    $ [hyperplaneFolder,"/",show k,"/hyperplane.txt"])
    [1..144]
  sigmas <- mapM (\k -> 
    loadSigma . concat 
    $ [hyperplaneFolder,"/",show k,"/hyperplane.txt"])
    [1..144]
  loadImage imgPath 
     >>= fromJust
     # chop 80 80 
     # fmap (imageToVector . changeResolution 20 20)
     # zipWith distance hyperplanes
     # fmap (^2)
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

loadSigma :: FilePath -> IO Double
loadSigma = undefined

