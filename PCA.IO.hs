module PCA.IO
( saveHyperplane
, loadHyperplane
, loadImages
, imagesToVector
) where

import PCA.Data
import System.Environment
import System.Directory

saveHyperplane :: FilePath -> Hyperplane -> IO ()
-- saves a hyperplane in machine-readable plaintext format
save f h = writeFile

loadHyperplane :: FilePath -> IO Hyperplane
-- loads a hyperplane that was saved in the syntax of `save`
load f = readFile f >>= (return . read)

