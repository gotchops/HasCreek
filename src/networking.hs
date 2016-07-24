import Network.Socket
import Network.HostName
import Network.Simple.TCP
import System.IO
import qualified Network.HTTP as HTTP

import PeerWireProtocol
import Tracker

type Port = String

getTrackerResponse :: String -> IO String
getTrackerResponse url = HTTP.simpleHTTP (HTTP.getRequest url) >>= HTTP.getResponseBody

peerServer :: Port -> IO ()
peerServer port = withSocketDo $ serve getHostName port servePeer

servePeer :: (Socket, SockAddr) -> IO ()
servePeer (sock, remoteAddr) = do
    hs <- recv sock 100
    case hs of Nothing -> closeSock sock
               Just b -> if isVerified hs 
                         then talkToPeer sock
                         else closeSock sock
               
talkToPeer :: Socket -> String -> String -> IO ()
talkToPeer sock ih pid = do
    sendHandshake sock ih pid
    sendBitfied sock
    handleMessages sock
    
sendHandshake :: Socket -> String -> String -> IO ()
sendHandshake sock ih pid = do 
    let hs = Handshake { pstrlen = BC.pack "19"
                       , pstr = BC.pack "BitTorrent protocol"
                       , reserved = BC.pack "00000000"
                       , infoHash = ih
                       , peerId = pid
                       }
    in send sock (runPut $ Put hs)
    

sendBitfield :: Socket -> IO ()
sendBitfield sock = do
    --get bitfied and send it
    
handleMessages :: Socket -> IO ()
handleMessages sock = do
    msg <- recv sock (2^15 + 100)
    case (runGet $ Get msg) of Interested -> do 
                                        handleInterested sock 
                                        handleMessage sock
                                     (Request i b l) -> do 
                                        handleRequest sock i b l
                                        handleMessage sock
                                     NotInterested -> closeSock sock
                                     _ -> closeSock sock
handleInterested :: Socket -> IO ()
handleInterested sock = do
    case isAnyPiece of True -> send sock (runPut $ Put Unchoke)
                       False -> send sock (runPut $ Put Choke)
                        
handleRequest :: Socket -> Word32 -> Word32 -> Word32 -> IO ()
handleRequest sock index begin length
    case isPiece index of True -> send sick (getPiece index begin length)
                          False -> return ()
                          
------------------------------------------------------
    