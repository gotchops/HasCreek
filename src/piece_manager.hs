module PieceManager where

import qualified Data.ByteString as B
import Data.Bits
import Data.Word
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan

bitfieldToList :: B.ByteString -> [Word8]
bitfieldToList bf = bftl 0 bf
    where bftl i b = case B.unpack b of [] -> []
                                        x:xs -> (map (+i) (have x)) ++ bftl (i+8) (B.tail b)
          
          have w = let bitsOn = map (\y -> testBit w y) [7,6..0]
                       indices lst acc iter = case lst of [] -> acc
                                                          h:t -> if h == True
                                                                then indices t (acc ++ [iter]) (iter + 1)
                                                                else indices t acc (iter + 1)
                   in indices bitsOn [] 0
                   
--listToBitfield :: [Word8] -> B.ByteString
--listToBitfield lst = ???

setDownloaded :: B.ByteString -> TChan B.ByteString -> STM ()
setDownloaded b chan = do empty <- isEmptyTChan chan
                          if empty then writeTChan chan b
                                   else do readTChan chan
                                           setDownloaded b chan

setUploaded :: Int -> TChan Int -> STM ()
setUploaded u chan = do empty <- isEmptyTChan chan
                        if empty then writeTChan chan u
                                 else do readTChan chan 
                                         setUploaded u chan

getDownloaded :: TChan B.ByteString -> STM Int
getDownloaded chan = do bf <- readTChan chan
                        return $ length (bitfieldToList bf)
                           
getUploaded :: TChan Int -> STM Int
getUploaded chan = do bf <- readTChan chan
                      return $ bf
                         