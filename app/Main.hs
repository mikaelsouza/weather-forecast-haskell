module Main where

import System.Environment (getArgs)
import Weather.WeatherAPI
import Weather.TUI
import Control.Exception (handle, SomeException)

main :: IO ()
main = handle errorHandler $ do
    args <- getArgs
    case args of
        [city] -> runWeather city
        _      -> putStrLn "Usage: weather <city>"

runWeather :: String -> IO ()
runWeather city = do
    putStrLn $ "Searching for " ++ city ++ "..."
    mCoords <- getCoords city
    case mCoords of
        Nothing -> putStrLn $ "Error: Could not find city '" ++ city ++ "'"
        Just coords -> do
            putStrLn "Fetching weather data..."
            mWeather <- getWeather coords
            case mWeather of
                Nothing -> putStrLn "Error: Could not fetch weather data"
                Just wd -> renderWeather wd

errorHandler :: SomeException -> IO ()
errorHandler e = do
    putStrLn "=========================================="
    putStrLn "                 ERROR"
    putStrLn "=========================================="
    putStrLn $ "An unexpected error occurred: " ++ show e
    putStrLn "Please check your internet connection."
    putStrLn "=========================================="
