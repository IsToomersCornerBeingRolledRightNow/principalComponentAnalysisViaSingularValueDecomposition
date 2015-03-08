import ImageToVector (loadImages, chop)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import Vision.Image (RGB)
import Vision.Image.Storage.DevIL (save, PNG(..))
import Vision.Primitive.Shape((:.),Z)
import qualified Data.Vector.Storable as V
import Data.List (elemIndices, splitAt)


--defaultImg :: RGB
--defaultImg = Manifest (Z :. 1 :. 1) (V.singleton (RGBPixel 0 0 0))

getDir :: FilePath -> FilePath
getDir = fst . splitFileName
getFileName :: FilePath -> FilePath
getFileName = snd . splitFileName

splitFileName :: FilePath -> (FilePath,FilePath)
splitFileName f = case (elemIndices '/' f) of
  [] -> (".", f)
  x -> splitAt (last x) f


main :: IO ()
main = do
  imagePath:_ <- getArgs
  print imagePath
  let imageFiles = [getFileName imagePath]
      dir = getDir imagePath
  maybeImages <- loadImages [imagePath]
  let images = [(x,a) | (x,Just a) <- zip imageFiles maybeImages]
  let pieces = (fmap . fmap) (chop 80 80) images
  mapM_ (uncurry $ savePieces dir) pieces
  

  --print $ manifestSize $ (last . head) pieces
  --print $ map length pieces

savePiece :: FilePath -> RGB -> IO ()
savePiece name img = save PNG name img >> return ()

savePieces :: FilePath -> String -> [RGB] -> IO ()
savePieces dir basename pieces = do
  let f n = concat [dir, "/chopped/", show n,"/"]
      paths = fmap f [1..(length pieces)]
      names = fmap (++basename) paths
  mapM_ (createDirectoryIfMissing True) paths
  mapM_ (uncurry $ save PNG) (zip names pieces)

