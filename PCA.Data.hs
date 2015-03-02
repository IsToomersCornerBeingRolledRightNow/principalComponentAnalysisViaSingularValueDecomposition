module PCA.Data
( Hyperplane
) where

import Data.Packed.Matrix
import Data.Packed.Vector

data Hyperplane = Hyperplane Int (Vector Double) (Matrix Double) deriving (Show, Read)

