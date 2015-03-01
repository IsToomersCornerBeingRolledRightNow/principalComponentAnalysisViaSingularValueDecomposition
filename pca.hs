import Numeric.LinearAlgebra.HMatrix
import Data.Packed.Matrix
import Data.Packed.Vector
import Data.List (transpose)

data Hyperplane = Hyperplane Int (Vector Double) (Matrix Double) deriving (Show)
  -- Vector Double should be the mean
  
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


mean :: Matrix Double -> [Double]
mean x = fmap (/n) s
  where 
  vs = toLists x
  s = fmap sum . transpose $ vs
  n = fromIntegral $ rows x

normalize :: Matrix Double -> Matrix Double
normalize x = x - (fromLists . take n $ repeat m)
  where
  m = mean x
  n = fromIntegral $ rows x

lsRegress :: [Vector Double] -> Int -> [Vector Double]
lsRegress = undefined

makePlane :: Matrix Double -> Double -> Hyperplane
makePlane mat cutoff = Hyperplane n m rows
  where
  n = cols mat
  m = fromList $ mean mat
  normalizedmat = normalize mat
  (_,rows) = trimSVDRight cutoff normalizedmat
  
trimSVDRight :: Double -> Matrix Double -> (Vector Double, Matrix Double)
trimSVDRight min inmat = (v, outmat)
  where
  (v1,m1) = rightSV inmat
  v = fromList . takeWhile (>= min) . toList $ v1
  outmat = trans $ takeColumns (dim v) m1
  
distance2 :: Vector Double -> Hyperplane -> Double
distance2 v (Hyperplane n hv m) = norm_2 $ distancemat #> v'
  where
  v' = v - hv
  distancemat = (trans m) * m - (ident n)
  
  
