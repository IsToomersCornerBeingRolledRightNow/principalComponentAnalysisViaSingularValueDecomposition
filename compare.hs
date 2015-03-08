import PCA (loadHyperplane, distance, Hyperplane)
import System.Environment (getArgs)
import ImageToVector 
  ( loadImageToVector
  , imageToVector
  , chop
  , loadImage 
  , changeResolution)
import Data.Maybe (fromJust)
import Control.Parallel.Strategies (parMap, rseq, rpar, rdeepseq)

main = do
  hyperplaneFolder:imgPath:_ <- getArgs
  hyperplanes <- mapM (\k -> 
    loadHyperplane . concat 
    $ [hyperplaneFolder,"/",show k,"/hyperplane.txt"])
    [1..144]
  sigmas <- mapM (\k -> 
    loadSigma . concat 
    $ [hyperplaneFolder,"/",show k,"/avgdist.txt"])
    [1..144]
  loadImage imgPath 
     >>= fromJust
     # chop 80 80 
     # fmap (imageToVector . changeResolution 20 20)
     # parZipWith distance hyperplanes
     # parZipWith (flip (/)) sigmas
     # fmap (^2)
     # sum
     # print

infixl 3 #
(#) :: (a -> b) -> (b -> c) -> (a -> c)
(#) = flip (.)

loadSigma :: FilePath -> IO Double
loadSigma f = fmap read $ readFile f

--parZipWith :: (NFData c) => (a -> b -> c) -> [a] -> [b] -> [c]
parZipWith f xs ys = parMap rpar (uncurry f) (zip xs ys)
