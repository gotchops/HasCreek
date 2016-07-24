module Bencode
( bencode
, parseBencode
, parseBencodedText
, Bencode (BString, BInteger, BList, BDictionary)
) where
 
import Text.ParserCombinators.Parsec
import Data.List
import Data.Word

data Bencode = BString String
             | BInteger Word
             | BList [Bencode]
             | BDictionary [(Bencode, Bencode)] deriving (Eq, Show, Read, Ord)

parseBencode :: Parser Bencode
parseBencode = parseBDictionary <|> parseBString <|> parseBInteger <|> parseBList

bencode :: Bencode -> String
bencode (BString s)  = (show $ length s) ++ ":" ++ s
bencode (BInteger i) = "i" ++ show i ++ "e"
bencode (BList []) = "le"
bencode (BList lst) = "l" ++ (intercalate "" (map bencode lst)) ++ "e" 
bencode (BDictionary []) = "de"
bencode (BDictionary d) = "d" ++ (intercalate "" (map (\x -> bencode (fst x) ++ bencode (snd x)) d)) ++ "e"

parseBencodedText :: String -> Either ParseError Bencode
parseBencodedText t = parse parseBencode "" t

parseBString :: Parser Bencode
parseBString = do d <- many1 digit
                  char ':'
                  c <- sequence (replicate (read d) anyChar) 
                  return $ BString c

parseBInteger :: Parser Bencode
parseBInteger = do char 'i'
                   d <- many digit
                   char 'e'
                   return $ BInteger (read d)

parseBList :: Parser Bencode
parseBList = do char 'l'
                e <- many parseBencode
                char 'e'
                return $ BList e

parseBDictionary :: Parser Bencode
parseBDictionary  = do char 'd'
                       d <- many (parseBString <|> parseBencode)
                       char 'e'
                       return $ BDictionary (listToPairList d)
    where listToPairList [] = []
          listToPairList (k:v:xs) = (k, v) : listToPairList xs
