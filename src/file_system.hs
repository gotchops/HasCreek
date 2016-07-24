module FileSystem where

import qualified Data.ByteString as B
import Data.Word
import System.IO

type Size = Int -- number of bytes
type Index = Int

data File = File { path :: String, size :: Int }

createFile :: FilePath -> Size -> IO ()
createFile path size = B.writeFile path (B.pack $ replicate size (0 :: Word8))

readFile :: FilePath -> Index -> IO (B.ByteString)
readFile path index = do withBinaryFile path ReadMode (\h -> do hSeek h AbsoluteSeek (toInteger index)
                                                                B.hGet h 1)

writeFile :: FilePath -> Index -> B.ByteString -> IO ()
writeFile path index piece = do withBinaryFile path WriteMode (\h -> do hSeek h AbsoluteSeek (toInteger index)
                                                                        B.hPut h piece)
       
       