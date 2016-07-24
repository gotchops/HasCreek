module Tracker where

import Data.Word
import Text.ParserCombinators.Parsec
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Crypto.Hash.SHA1 as SHA1

import Metainfo
import Bencode

data TrackerRequest = TrackerRequest { infoHash :: String
                                     , peerId :: String
                                     , port :: String
                                     , uploaded :: String
                                     , downloaded :: String
                                     , left :: String
                                     , compact :: String
                                     , event :: String
                                     } deriving (Show)

data TrackerResponse = TrackerResponse { interval :: Word
                                       , trackerId :: String
                                       , complete :: Word
                                       , incomplete :: Word
                                       , peers :: Peers 
                                       } deriving (Show)
                                        

data Peers = DModel [(BC.ByteString, BC.ByteString, Word)] | BModel [(BC.ByteString, BC.ByteString)] deriving (Show)

createTrackerRequest :: String -> [(String, String)] -> String
createTrackerRequest announce params = (HTTP.urlEncode announce) ++ "?" ++ (HTTP.urlEncodeVars params)

createParams :: TrackerRequest -> [(String, String)]
createParams (TrackerRequest{ infoHash = ih
                            , peerId = pid
                            , port = p
                            , uploaded = u
                            , downloaded = d
                            , left = l
                            , compact = c
                            , event = e
                            }) = [ ("info_hash", ih)
                                 , ("peer_id", pid)
                                 , ("port", p)
                                 , ("uploaded", u)
                                 , ("downloaded", d)
                                 , ("left", l)
                                 , ("compact", c)
                                 , ("event", e)
                                 ]
                                 
createInfoHash :: String -> B.ByteString
createInfoHash s = SHA1.hash (BC.pack s)

createInfoHashString :: String -> String
createInfoHashString s = BC.unpack $ createInfoHash s

getPeerId = "-6T0001-963870461854"

getPort = "6882"

getTrackerResponse :: String -> IO String
getTrackerResponse url = HTTP.simpleHTTP (HTTP.getRequest url) >>= HTTP.getResponseBody

parseTrackerResponse :: String -> Either ParseError Bencode
parseTrackerResponse res = parseBencodedText res

trackerResponse :: Bencode -> String -> Maybe TrackerResponse
trackerResponse d tid = do i <- getInterval d
                           id <- case (getTrackerId d) of Nothing -> Just tid
                                                          Just s -> Just s
                           c <- getComplete d
                           ic <- getIncomplete d
                           p <- getPeers d
                           return (TrackerResponse {interval = i, trackerId = id, complete = c, incomplete = ic, peers = p})

getInterval :: Bencode -> Maybe Word
getInterval d = lookupInteger "interval" d

getTrackerId :: Bencode -> Maybe String
getTrackerId d = lookupString "tracker id" d

getComplete :: Bencode -> Maybe Word
getComplete d = lookupInteger "complete" d

getIncomplete :: Bencode -> Maybe Word
getIncomplete d = lookupInteger "incomplete" d

getPeers :: Bencode -> Maybe Peers
getPeers d = case d of (BList p) -> do lst <- dicF p
                                       return $ DModel lst
                       (BString p) -> Just (BModel (binF (BC.pack p)))
                       _ -> Nothing
    where dicF ((BDictionary d):ds) = do pid <- lookupString "peer id" (BDictionary d) 
                                         ip <- lookupString "ip" (BDictionary d) 
                                         port <- lookupInteger "port" (BDictionary d) 
                                         rest <- dicF ds
                                         return $ [(BC.pack pid, BC.pack ip, port)] ++ rest
          dicF _ = Just []
     
          binF :: BC.ByteString -> [(BC.ByteString, BC.ByteString)]    
          binF s
              | (BC.null s) = []
              | ((BC.length s) `mod` 6 /= 0) = []
              | otherwise = (BC.splitAt 4 (BC.take 6 s)):(binF $ BC.drop 6 s)
