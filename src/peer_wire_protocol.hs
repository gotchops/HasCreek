module PeerWireProtocol where

import qualified Data.ByteString as B
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get

data Handshake = Handshake { pstrlen :: Word8
                           , pstr :: B.ByteString
                           , reserved :: Word64
                           , infoHash :: Word160
                           , peerId :: Word160 }
                           
data Word160 = Word160 (Word8, Word8, Word8, Word8,
                        Word8, Word8, Word8, Word8,
                        Word8, Word8, Word8, Word8,
                        Word8, Word8, Word8, Word8,
                        Word8, Word8, Word8, Word8)
                        
instance Binary Word160 where
    put (Word160 (e0,e1,e2,e3,
                  e4,e5,e6,e7,
                  e8,e9,e10,e11,
                  e12,e13,e14,e15,
                  e16,e17,e18,e19)) = do put e0
                                         put e1
                                         put e2
                                         put e3
                                         put e4
                                         put e5
                                         put e6
                                         put e7
                                         put e8
                                         put e9
                                         put e10
                                         put e11
                                         put e12
                                         put e13
                                         put e14
                                         put e15
                                         put e16
                                         put e17
                                         put e18
                                         put e19
                                                                                                    
    get = do e0 <- getWord8
             e1 <- getWord8
             e2 <- getWord8
             e3 <- getWord8
             e4 <- getWord8
             e5 <- getWord8
             e6 <- getWord8
             e7 <- getWord8
             e8 <- getWord8
             e9 <- getWord8
             e10 <- getWord8
             e11 <- getWord8
             e12 <- getWord8
             e13 <- getWord8
             e14 <- getWord8
             e15 <- getWord8
             e16 <- getWord8
             e17 <- getWord8
             e18 <- getWord8
             e19 <- getWord8
             return $ Word160 (e0,e1,e2,e3,
                               e4,e5,e6,e7,
                               e8,e9,e10,e11,
                               e12,e13,e14,e15,
                               e16,e17,e18,e19)
                               
instance Binary Handshake where
    put (Handshake {pstrlen = pl
                   , pstr = p
                   , reserved = r
                   , infoHash = i
                   , peerId = id
                   }) = do put p
                           putByteString p
                           put r
                           put i
                           put id
                           
    get = do pl <- getWord8
             p <- return $ sequence (replicate (fromIntegral $ pl) (getWord8))
             pp <- p
             r <- getWord64be
             i <- get :: Get Word160
             pi<- get :: Get Word160
             return $ Handshake {pstrlen = pl, pstr = (B.pack pp), reserved = r, infoHash = i, peerId = pi}

data Message = KeepAlive
             | Choke
             | Unchoke
             | Interested
             | NotInterested
             | Have Word32
             | Bitfield B.ByteString
             | Request Word32 Word32 Word32
             | Piece Word32 Word32 B.ByteString
             | Cancel Word32 Word32 Word32
                      
instance Binary Message where
    put KeepAlive = put (0 :: Word32)
    put Choke = do put (1 :: Word32)
                   put (0 :: Word8)
    put Unchoke = do put (1 :: Word32)
                     put (1 :: Word8)
    put Interested = do put (1 :: Word32)
                        put (2 :: Word8)
    put NotInterested = do put (1 :: Word32)
                           put (3 :: Word8)
    put (Have pieceIndex) = do put (5 :: Word32)
                               put (4 :: Word8)
                               put pieceIndex
    put (Bitfield bitfield) = do put (fromIntegral $ 1 + B.length bitfield :: Word32)
                                 put (4 :: Word8)
                                 putByteString bitfield
    put (Request index begin length) = do put (13 :: Word32)
                                          put (6 :: Word8)
                                          put index
                                          put begin
                                          put length
    put (Piece index begin block) = do put (fromIntegral $ 9 + B.length block :: Word32)
                                       put (7 :: Word8)
                                       put index
                                       put begin
                                       putByteString block
                                        
    put (Cancel index begin length) = do put (13 :: Word32)
                                         put (8 :: Word8)
                                         put index
                                         put begin
                                         put length
                      
    get = do length <- getWord32be
             case length of 0 -> return $ KeepAlive
                            _ -> do id <- getWord8
                                    case id of 0 -> return $ Choke
                                               1 -> return $ Unchoke
                                               2 -> return $ Interested
                                               3 -> return $ NotInterested
                                               4 -> do pieceIndex <- getWord32be
                                                       return $ Have pieceIndex 
                                               5 -> do bitfield <- getByteString (fromIntegral length - 1)
                                                       return $ Bitfield bitfield
                                               6 -> do index <- getWord32be
                                                       begin <- getWord32be
                                                       len <- getWord32be
                                                       return $ Request index begin len
                                               7 -> do index <- getWord32be
                                                       begin <- getWord32be
                                                       block <- getByteString (fromIntegral length - 9)
                                                       return $ Piece index begin block
                                               8 -> do index <- getWord32be
                                                       begin <- getWord32be
                                                       len <- getWord32be
                                                       return $ Cancel index begin len
                                               _ -> fail "Message outside of this protocol."