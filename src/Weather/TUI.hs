module Weather.TUI where

import Weather.WeatherAPI
import Data.List (intercalate)
import System.IO (hFlush, stdout)

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

drawHeader :: String -> IO ()
drawHeader city = do
    putStrLn $ cyan (replicate 65 '=')
    putStrLn $ cyan "      7-Day Weather: " ++ white (padRight 40 city)
    putStrLn $ cyan (replicate 65 '=')
    putStrLn " Date       | Max Temp          | Min Temp          | Condition"
    putStrLn "------------|-------------------|-------------------|---------------"

drawForecast :: Forecast -> IO ()
drawForecast f = do
    let cond = weatherCondition (weatherCode f)
        c = conditionColor (weatherCode f)
        maxC = round (maxTempC f)
        maxF = round (maxTempF f)
        minC = round (minTempC f)
        minF = round (minTempF f)
        maxStr = padRight 6 (show maxC ++ "°C") ++ " (" ++ padLeft 3 (show maxF) ++ "°F)"
        minStr = padRight 6 (show minC ++ "°C") ++ " (" ++ padLeft 3 (show minF) ++ "°F)"
    putStrLn $ padRight 11 (date f) ++ " | " ++
              yellow (padRight 17 maxStr) ++ " | " ++
              blue (padRight 17 minStr) ++ " | " ++
              c cond

conditionColor :: Int -> (String -> String)
conditionColor code = case code of
    0 -> green
    c | c >= 1 && c <= 2 -> yellow
    3 -> white
    c | c >= 45 && c <= 48 -> white
    c | c >= 51 && c <= 67 -> blue
    c | c >= 71 && c <= 77 -> white
    c | c >= 80 && c <= 86 -> blue
    c | c >= 95 && c <= 99 -> magenta
    _ -> white

-- ANSI Colors
red, green, yellow, blue, magenta, cyan, white :: String -> String
red s = "\ESC[31m" ++ s ++ "\ESC[0m"
green s = "\ESC[32m" ++ s ++ "\ESC[0m"
yellow s = "\ESC[33m" ++ s ++ "\ESC[0m"
blue s = "\ESC[34m" ++ s ++ "\ESC[0m"
magenta s = "\ESC[35m" ++ s ++ "\ESC[0m"
cyan s = "\ESC[36m" ++ s ++ "\ESC[0m"
white s = "\ESC[37m" ++ s ++ "\ESC[0m"

weatherCondition :: Int -> String
weatherCondition code = case code of
    0 -> "Clear sky"
    1 -> "Mainly clear"
    2 -> "Partly cloudy"
    3 -> "Overcast"
    45 -> "Fog"
    48 -> "Depositing rime fog"
    51 -> "Drizzle: Light"
    53 -> "Drizzle: Moderate"
    55 -> "Drizzle: Dense"
    56 -> "Freezing Drizzle: Light"
    57 -> "Freezing Drizzle: Dense"
    61 -> "Rain: Slight"
    63 -> "Rain: Moderate"
    65 -> "Rain: Heavy"
    66 -> "Freezing Rain: Light"
    67 -> "Freezing Rain: Heavy"
    71 -> "Snow fall: Slight"
    73 -> "Snow fall: Moderate"
    75 -> "Snow fall: Heavy"
    77 -> "Snow grains"
    80 -> "Rain showers: Slight"
    81 -> "Rain showers: Moderate"
    82 -> "Rain showers: Violent"
    85 -> "Snow showers: Slight"
    86 -> "Snow showers: Heavy"
    95 -> "Thunderstorm"
    96 -> "Thunderstorm: Slight hail"
    99 -> "Thunderstorm: Heavy hail"
    _ -> "Unknown (" ++ show code ++ ")"

padRight :: Int -> String -> String
padRight n s = s ++ replicate (n - length s) ' '

padLeft :: Int -> String -> String
padLeft n s = replicate (n - length s) ' ' ++ s

renderWeather :: WeatherData -> IO ()
renderWeather wd = do
    clearScreen
    drawHeader (cityName wd)
    mapM_ drawForecast (forecasts wd)
    putStrLn $ cyan (replicate 65 '=')
    hFlush stdout
