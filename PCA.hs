module PCA
( Hyperplane (..)
, saveHyperplane
, loadHyperplane
, linRegression
, distance2
) where

import Numeric.LinearAlgebra.HMatrix
import Numeric.LinearAlgebra.Data
import Data.Packed.Matrix
import Data.Packed.Vector

data Hyperplane = Hyperplane Int (Vector Double) (Matrix Double) deriving (Show, Read)
-- First argument is the dimension of the ambient space.
-- Second argument is some vector in the hyperplane.
-- Rows of third argument form the basis of the parallel
-- linear subspace.
-- The Hyperplane is the coset of the second arg mod the third arg.

saveHyperplane :: FilePath -> Hyperplane -> IO ()
-- Saves a hyperplane in machine-readable plaintext format.
saveHyperplane f h = writeFile f (show h)

loadHyperplane :: FilePath -> IO Hyperplane
-- Loads a hyperplane that was saved in the syntax of `save`.
loadHyperplane f = readFile f >>= (return . read)

mean :: Matrix Double -> Vector Double
-- Returns the average of the rows of a matrix.
mean x = scale n . sum . toRows $ x
  where
  n = 1 / (fromIntegral . rows $ x) :: Double

normalize :: Matrix Double -> Matrix Double
-- Subtracts the average of/from the rows of a matrix.
normalize x = x - (fromRows . replicate (rows x) . mean $ x)

linRegression :: Double -> Matrix Double -> Hyperplane
-- Finds the best-fit hyperplane of the rows of a matrix.
-- First argument is the threshold to use a singular value.
-- Second argument is your data, rows are individual datapoints.
-- The first argument controls the size of the hyperplane.
-- If the first argument is too small, the hyperplane will just be
-- the whole space. If the first argument is too big, the hyper-
-- plane will just be a single point.
linRegression min inmat = Hyperplane n m rows
  where
  n = cols inmat
  m = mean inmat
  normalizedmat = normalize inmat
  (_,rows) = trimSVDRight min normalizedmat

trimSVDRight :: Double -> Matrix Double -> (Vector Double, Matrix Double)
-- Returns partial diagonal and right-side factor from SVD of
-- a matrix.
-- First argument is the threshold to use a singular value
-- (setting to 0 will return all singular values).
-- Second argument is a matrix.
trimSVDRight min inmat = (v, outmat)
  where
  (v1,m1) = rightSV inmat
  v = fromList . takeWhile (>= min) . toList $ v1
  outmat = trans $ takeColumns (dim v) m1

distance2 :: Hyperplane -> Vector Double -> Double
-- Finds the square of the distance from a vector to a hyperplane.
distance2 (Hyperplane n hv m) v 
  | cols m == 0 = norm_2 v'
  | rows m == 0 = norm_2 v'
  | otherwise   = norm_2 $ distancemat #> v'
  where
  v' = v - hv
  distancemat = (trans m) * m - (ident n)

