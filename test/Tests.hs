module Main where

import Weather.JsonParser
import Weather.WeatherAPI
import System.Exit (exitFailure, exitSuccess)
import Control.Monad (when)

test :: String -> Bool -> IO ()
test name True = putStrLn $ "[PASS] " ++ name
test name False = do
    putStrLn $ "[FAIL] " ++ name
    exitFailure

main :: IO ()
main = do
    putStrLn "Running Tests..."
    
    test "Parse null" $ parseJson "null" == Just JsonNull
    test "Parse bool true" $ parseJson "true" == Just (JsonBool True)
    test "Parse bool false" $ parseJson "false" == Just (JsonBool False)
    test "Parse number" $ parseJson "123.45" == Just (JsonNumber 123.45)
    test "Parse string" $ parseJson "\"hello\"" == Just (JsonString "hello")
    test "Parse array" $ parseJson "[1, 2, 3]" == Just (JsonArray [JsonNumber 1, JsonNumber 2, JsonNumber 3])
    test "Parse object" $ parseJson "{\"key\": \"val\"}" == Just (JsonObject [("key", JsonString "val")])
    
    test "Fail: Malformed JSON" $ parseJson "{" == Nothing
    test "Fail: Unclosed string" $ parseJson "\"abc" == Nothing
    test "Fail: Extra comma in array" $ parseJson "[1,]" == Nothing
    
    let mockGeocode = "{\"results\":[{\"name\":\"Berlin\",\"latitude\":52.52,\"longitude\":13.41}]}"
    test "Parse Coords Success" $ parseCoords mockGeocode == Just ("Berlin", 52.52, 13.41)
    
    let mockGeocodeEmpty = "{\"results\":[]}"
    test "Parse Coords Empty" $ parseCoords mockGeocodeEmpty == Nothing
    
    let mockWeather = "{\"daily\":{\"time\":[\"2024-01-01\"],\"weather_code\":[0],\"temperature_2m_max\":[10.0],\"temperature_2m_min\":[5.0]}}"
    test "Parse Weather Success" $ case parseWeather "Berlin" mockWeather of
        Just wd -> case forecasts wd of
            (f:_) -> length (forecasts wd) == 1 && cityName wd == "Berlin" && maxTempC f == 10.0 && maxTempF f == 50.0
            [] -> False
        Nothing -> False

    let mockWeatherMissing = "{\"daily\":{}}"
    test "Parse Weather Fail (Missing fields)" $ parseWeather "Berlin" mockWeatherMissing == Nothing

    putStrLn "All tests passed!"
    exitSuccess
