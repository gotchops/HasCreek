module Metainfo
( Metainfo
, Info
, FileInfo (Single, Multi)
, createMetainfo
) where

import Bencode
import Data.List
import qualified Data.ByteString.Char8 as BC

data Metainfo = Metainfo { info :: Info
                         , announce :: String 
                         } deriving (Show)
                         
data Info = Info { pieceLength :: Word
                 , pieces :: [BC.ByteString]
                 , fileInfo :: FileInfo
                 } deriving (Show)
          
data FileInfo = Single String Word 
              | Multi String [File] deriving (Show)

type File = (Word, String)

createMetainfo :: Bencode -> Maybe Metainfo
createMetainfo meta@(BDictionary d) = do infoDict <- lookupDictionary "info" meta
                                         i <- createInfo (BDictionary infoDict)
                                         a <- lookupString "announce" meta
                                         return $ Metainfo { info = i, announce = a }
createMetainfo _ = Nothing

createInfo :: Bencode -> Maybe Info
createInfo infoDict@(BDictionary d) = do pl <- lookupInteger "piece length" infoDict
                                         piecesStr <- lookupString "pieces" infoDict
                                         p <- Just (bsToList (fromIntegral pl) (BC.pack piecesStr))
                                         fi <- createFileInfo infoDict
                                         return $ Info { pieceLength = pl, pieces = p, fileInfo = fi }
createInfo _ = Nothing

bsToList :: Int -> BC.ByteString -> [BC.ByteString]
bsToList len str = if BC.null str then []
                                  else (BC.take len str):(bsToList len (BC.drop len str))

createFileInfo :: Bencode -> Maybe FileInfo
createFileInfo infoDict@(BDictionary d) = let len = lookupInteger "length" infoDict
                                          in case len of Nothing -> createMulti infoDict
                                                         _ -> createSingle infoDict
createFileInfo _ = Nothing

createSingle :: Bencode -> Maybe FileInfo
createSingle infoDict@(BDictionary d) = do name <- return $ getName "CTFile" infoDict
                                           length <- lookupInteger "length" infoDict
                                           return $ Single name length
createSingle _ = Nothing

createMulti :: Bencode -> Maybe FileInfo
createMulti infoDict@(BDictionary d) = do name <- return $ getName "." infoDict
                                          f <- (lookupList "files" infoDict)
                                          files <- return $ parseFiles (map parseFile f)
                                          return $ Multi name files 
    where parseFiles [] = []
          parseFiles (f:fs) = case f of Nothing -> parseFiles fs
                                        (Just file) -> [file] ++ parseFiles fs
                        
          parseFile file@(BDictionary d) = do length <- lookupInteger "length" file
                                              rawPath <- lookupList "path" file
                                              path <- return $ parsePath rawPath
                                              return (length, path)
          parseFile _ = Nothing
          
          parsePath lst = blistToString (intersperse (BString "/") lst)
          
          blistToString ((BString s):ss) = s ++ (blistToString ss)
          blistToString _ = ""
createMulti _ = Nothing

getName :: String -> Bencode -> String
getName dummy dict = case (lookupString "name" dict) of Nothing -> dummy
                                                        Just s -> s

lookupString :: String -> Bencode -> Maybe String
lookupString k (BDictionary i) = let p = find (\x -> fst x == (BString k)) i
                                 in case p of Just (_, BString v) -> Just v
                                              _ -> Nothing
lookupString _ _ = Nothing

lookupInteger :: String -> Bencode -> Maybe Word
lookupInteger k (BDictionary i) = let p = find (\x -> fst x == (BString k)) i
                                 in case p of Just (_, BInteger v) -> Just v
                                              _ -> Nothing
lookupInteger _ _ = Nothing

lookupList :: String -> Bencode -> Maybe [Bencode] 
lookupList k (BDictionary i) = let p = find (\x -> fst x == (BString k)) i
                                 in case p of Just (_, BList v) -> Just v
                                              _ -> Nothing
lookupList _ _ = Nothing

lookupDictionary :: String -> Bencode -> Maybe [(Bencode, Bencode)] 
lookupDictionary k (BDictionary i) = let p = find (\x -> fst x == (BString k)) i
                                 in case p of Just (_, BDictionary v) -> Just v
                                              _ -> Nothing
lookupDictionary _ _ = Nothing
