import Codec.G56
import System.IO
import System.Environment
import qualified Data.ByteString as B
import Data.List.Split (chunksOf)

main = do
   args <- getArgs
   case args of
      [] -> putStr =<< unlines . chunksOf 77 . encode <$> B.getContents
      ["-d"] -> B.putStr =<< decode <$> getContents
      _ -> hPutStrLn stderr "Usage: g56 [-d] < input > output"

