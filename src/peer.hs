import Network.Connection

talkToPeer :: String -> Int -> IO ()
talkToPeer host port = do context <- initConnectionContext
                    con <- connectTo context $ ConnectionParams
                                                       { connectionHostname = host
                                                       , connectionPort = port
                                                       , connectionUseSecure = Nothing
                                                       , connectionUseSocks = Nothing
                                                       }
                    connectionPut con (encode $ createHandshake getInfoHash getPeerId)
                    hshake <- connectionGet cont 100
                    verification <- verifyHandshake $ decode hshake
                                                   
createHandshake :: Word160 -> Word160 -> Handshake
createHandshake ih pid = Handshake { pstrlen = BC.pack "19"
                                   , pstr = BC.pack "BitTorrent protocol"
                                   , reserved = BC.pack "00000000"
                                   , infoHash = ih
                                   , peerId = pid
                                   }
                                   
verifyHandshake
                    


