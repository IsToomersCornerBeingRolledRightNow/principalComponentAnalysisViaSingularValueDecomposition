import PCA (loadHyperplane, distance)
import System.Environment (getArgs)
import ImageToVector (loadImageToVector)

main :: IO ()
main = do
  [imagePath, hyperplanePath] <- getArgs
  Just i <- loadImageToVector 20 20 imagePath
  h <- loadHyperplane hyperplanePath
  print $ distance h i
