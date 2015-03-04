{-|
main :: IO ()
main = do
  [dir, height, width, threshold] <- getArgs
  let h' = read height
  let w' = read width
  let t' = read threshold
  imageFiles <- getDirectoryContents dir
  let imagePaths = map (("./" ++ dir) ++) imageFiles
  maybeImages <- loadImages imagePaths h' w'
  let rows = [a | Just a <- maybeImages]
  let matrix = fromRows rows
  let linreg = makePlane matrix t'
  print linreg  
-}  


{-|
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
-}
