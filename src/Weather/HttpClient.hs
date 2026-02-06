{-# LANGUAGE OverloadedStrings #-}

module Weather.HttpClient where

import Weather.SocketTransport
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Control.Exception (bracket)

data HttpResponse = HttpResponse
    { statusCode :: Int
    , headers    :: [(String, String)]
    , body       :: ByteString
    } deriving (Show)

httpGet :: String -> String -> IO HttpResponse
httpGet host path = bracket openSocket closeSocket $ \sock -> do
    connectTo sock host 80
    let request = B.concat
            [ "GET ", BC.pack path, " HTTP/1.1\r\n"
            , "Host: ", BC.pack host, "\r\n"
            , "User-Agent: HaskellWeather/1.0\r\n"
            , "Connection: close\r\n"
            , "\r\n"
            ]
    sendAll sock request
    response <- receiveAll sock
    let (hPart, bPart) = splitAtDoubleNewline response
        hLines = BC.lines hPart
        hdrs = map parseHeader (drop 1 hLines)
        isChunked = lookup "Transfer-Encoding" hdrs == Just "chunked"
        finalBody = if isChunked then decodeChunks bPart else bPart
    return $ parseResponse hPart finalBody

parseResponse :: ByteString -> ByteString -> HttpResponse
parseResponse headerPart bodyPart =
    let headerLines = BC.lines headerPart
        statusLine = case headerLines of
            (l:_) -> l
            [] -> ""
        code = parseStatusCode statusLine
        hdrs = map parseHeader (drop 1 headerLines)
    in HttpResponse code hdrs bodyPart

decodeChunks :: ByteString -> ByteString
decodeChunks bs
    | B.null bs = B.empty
    | otherwise =
        let (sizeLine, rest) = BC.break (== '\r') bs
            size = case readHex (BC.unpack sizeLine) of
                [(n, _)] -> n
                _ -> 0
            chunkData = B.take size (B.drop 2 rest)
        in if size == 0
           then B.empty
           else B.append chunkData (decodeChunks (B.drop (size + 4) rest))

readHex :: String -> [(Int, String)]
readHex s = case Prelude.span isHexDigit s of
    ("", _) -> []
    (h, r) -> [(Prelude.foldl (\acc c -> acc * 16 + digitToInt c) 0 h, r)]
  where
    isHexDigit c = c `elem` ("0123456789abcdefABCDEF" :: String)
    digitToInt c
        | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
        | c >= 'a' && c <= 'f' = fromEnum c - fromEnum 'a' + 10
        | c >= 'A' && c <= 'F' = fromEnum c - fromEnum 'A' + 10
        | otherwise = 0

splitAtDoubleNewline :: ByteString -> (ByteString, ByteString)
splitAtDoubleNewline bs =
    case findDoubleNewline 0 bs of
        Nothing -> (bs, B.empty)
        Just n  -> (B.take n bs, B.drop (n + 4) bs)
  where
    findDoubleNewline n b
        | n + 4 > B.length b = Nothing
        | B.index b n == 13 && B.index b (n+1) == 10 && B.index b (n+2) == 13 && B.index b (n+3) == 10 = Just n
        | otherwise = findDoubleNewline (n + 1) b

parseStatusCode :: ByteString -> Int
parseStatusCode line =
    let parts = BC.words line
    in if length parts >= 2
       then read (BC.unpack (parts !! 1))
       else 0

parseHeader :: ByteString -> (String, String)
parseHeader line =
    let (key, valPart) = BC.break (== ':') line
        val = B.drop 1 valPart
    in (BC.unpack (B.intercalate "" (BC.words key)), BC.unpack (B.intercalate " " (BC.words val)))
