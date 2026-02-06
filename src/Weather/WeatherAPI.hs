{-# LANGUAGE OverloadedStrings #-}

module Weather.WeatherAPI where

import Weather.HttpClient
import Weather.JsonParser
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (fromMaybe)
import Data.Char (isAlphaNum, intToDigit, ord)
import Numeric (showIntAtBase)

data Forecast = Forecast
    { date :: String
    , maxTempC :: Double
    , minTempC :: Double
    , maxTempF :: Double
    , minTempF :: Double
    , weatherCode :: Int
    } deriving (Show, Eq)

data WeatherData = WeatherData
    { cityName :: String
    , forecasts :: [Forecast]
    } deriving (Show, Eq)

getCoords :: String -> IO (Maybe (String, Double, Double))
getCoords city = do
    let path = "/v1/search?name=" ++ urlEncode city ++ "&count=1&language=en&format=json"
    resp <- httpGet "geocoding-api.open-meteo.com" path
    if statusCode resp /= 200
        then return Nothing
        else return $ parseCoords (BC.unpack (body resp))

urlEncode :: String -> String
urlEncode [] = []
urlEncode (c:cs)
    | isAlphaNum c = c : urlEncode cs
    | c == ' '     = '+' : urlEncode cs
    | otherwise    = '%' : hex (ord c) ++ urlEncode cs
  where
    hex n = let s = showIntAtBase 16 intToDigit n ""
            in if length s == 1 then '0' : s else s

parseCoords :: String -> Maybe (String, Double, Double)
parseCoords jsonStr = do
    val <- parseJson jsonStr
    case val of
        JsonObject obj -> do
            JsonArray results <- lookup "results" obj
            case results of
                [] -> Nothing
                (JsonObject first : _) -> do
                    JsonString name <- lookup "name" first
                    JsonNumber lat <- lookup "latitude" first
                    JsonNumber lon <- lookup "longitude" first
                    return (name, lat, lon)
                _ -> Nothing
        _ -> Nothing

getWeather :: (String, Double, Double) -> IO (Maybe WeatherData)
getWeather (name, lat, lon) = do
    let path = "/v1/forecast?latitude=" ++ show lat ++ "&longitude=" ++ show lon ++ "&daily=weather_code,temperature_2m_max,temperature_2m_min&timezone=auto"
    resp <- httpGet "api.open-meteo.com" path
    if statusCode resp /= 200
        then return Nothing
        else return $ parseWeather name (BC.unpack (body resp))

parseWeather :: String -> String -> Maybe WeatherData
parseWeather city jsonStr = do
    val <- parseJson jsonStr
    case val of
        JsonObject obj -> do
            JsonObject daily <- lookup "daily" obj
            JsonArray times <- lookup "time" daily
            JsonArray codes <- lookup "weather_code" daily
            JsonArray maxs <- lookup "temperature_2m_max" daily
            JsonArray mins <- lookup "temperature_2m_min" daily
            
            let fs = [ let ma = unNum max_c
                           mi = unNum min_c
                       in Forecast (unString t) ma mi (cToF ma) (cToF mi) (round (unNum c))
                     | (t, max_c, min_c, c) <- zip4 times maxs mins codes ]
            return $ WeatherData city fs
        _ -> Nothing

cToF :: Double -> Double
cToF c = c * 9 / 5 + 32

unString (JsonString s) = s
unString _ = ""
unNum (JsonNumber n) = n
unNum _ = 0.0

zip4 (a:as) (b:bs) (c:cs) (d:ds) = (a,b,c,d) : zip4 as bs cs ds
zip4 _ _ _ _ = []
