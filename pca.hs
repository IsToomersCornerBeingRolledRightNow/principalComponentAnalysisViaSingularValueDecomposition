import Numeric.LinearAlgebra.HMatrix
import Data.Packed.Matrix
import Data.Packed.Vector
import ImageToVector (loadImages)
import System.Environment (getArgs)
import System.Directory (getDirectoryContents)

data Hyperplane = Hyperplane Int (Vector Double) (Matrix Double) deriving (Show, Read)
  -- first argument is the dimension of the ambient space
  -- second argument is a vector in the hyperplane
  -- rows of third argument is the basis of the parallel linear subspace

mean :: Matrix Double -> [Double]
-- returns the average of the rows of a matrix
mean x = fmap (/n) s
  where 
  vs = toLists . trans $ x
  s = fmap sum vs
  n = fromIntegral . rows $ x

normalize :: Matrix Double -> Matrix Double
-- subtracts the average of/from the rows of a matrix
normalize x = x - (fromLists . take n $ repeat m)
  where
  m = mean x
  n = fromIntegral . rows $ x

makePlane :: Matrix Double -> Double -> Hyperplane
-- finds the best-fit hyperplane of the rows of a matrix
-- first argument is your data, rows are individual datapoints
-- second argument is the threshold SVD we want
makePlane inmat cutoff = Hyperplane n m rows
  where
  n = cols inmat
  m = fromList . mean $ inmat
  normalizedmat = normalize inmat
  (_,rows) = trimSVDRight cutoff normalizedmat

trimSVDRight :: Double -> Matrix Double -> (Vector Double, Matrix Double)
-- returns part of the diagonal and right-side factor from SVD of a matrix
-- first argument is the minimum required to return a sigular value
-- second argument is the matrix
trimSVDRight min inmat = (v, outmat)
  where
  (v1,m1) = rightSV inmat
  v = fromList . takeWhile (>= min) . toList $ v1
  outmat = trans $ takeColumns (dim v) m1

distance2 :: Vector Double -> Hyperplane -> Double
-- finds the square of the distance from a vector to a hyperplane
distance2 v (Hyperplane n hv m) = norm_2 $ distancemat #> v'
  where
  v' = v - hv
  distancemat = (trans m) * m - (ident n)
  
save :: FilePath -> Hyperplane -> IO ()
-- saves a hyperplane in machine-readable plaintext format
save f h = writeFile f (show h)

load :: FilePath -> IO Hyperplane
-- loads a hyperplane that was saved in the syntax of `save`
load f = readFile f >>= (return . read)

main :: IO ()
main = do
  [dir, height, width, threshold] <- getArgs
  let h' = read height
  let w' = read width
  let t' = read threshold
  imageFiles <- getDirectoryContents dir
  let imagePaths = map (("./" ++ dir) ++) imageFiles
  matrix <- fmap fromRows $ loadImages imagePaths h' w'
  let linreg = makePlane matrix t'
  print linreg
  
  



-- EXAMPLE DATA  
ex1 :: Matrix Double
ex1 =
  (4><2)
  [ 1.0, 0.0
  , 0.0, 1.0
  , 1.5, 0.5
  , 0.5, 1.5 ]

ex2 :: Matrix Double
ex2 = 
  (3><6)
  [ 1, 0, 0, 0, 0, 0
  , 2, 0, 0, 0, 0, 0
  , 3, 0, 0, 0, 0, 0 ]

ex3_h :: Hyperplane
ex3_h = Hyperplane 3 (fromList [1,1,0]) (fromLists [[1,0,0]])

ex3_v :: Vector Double
ex3_v = fromList [1,2,3]
-- END EXAMPLE DATA

