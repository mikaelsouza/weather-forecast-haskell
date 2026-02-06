module Weather.JsonParser where

import Data.Char (isDigit, isSpace)
import Control.Applicative

data JsonValue
    = JsonObject [(String, JsonValue)]
    | JsonArray [JsonValue]
    | JsonString String
    | JsonNumber Double
    | JsonBool Bool
    | JsonNull
    deriving (Show, Eq)

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \s -> do
        (x, s') <- p s
        return (f x, s')

instance Applicative Parser where
    pure x = Parser $ \s -> Just (x, s)
    (Parser p1) <*> (Parser p2) = Parser $ \s -> do
        (f, s') <- p1 s
        (x, s'') <- p2 s'
        return (f x, s'')

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \s -> p1 s <|> p2 s
    many p = some p <|> pure []
    some p = (:) <$> p <*> many p

instance Monad Parser where
    return = pure
    (Parser p) >>= f = Parser $ \s -> do
        (x, s') <- p s
        runParser (f x) s'

instance MonadFail Parser where
    fail _ = Parser $ const Nothing

charP :: Char -> Parser Char
charP c = Parser $ \s -> case s of
    (x:xs) | x == c -> Just (c, xs)
    _ -> Nothing

stringP :: String -> Parser String
stringP = sequenceA . map charP

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \s ->
    let (match, rest) = span f s
    in Just (match, rest)

ws :: Parser String
ws = spanP isSpace

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ stringP "null"

jsonBool :: Parser JsonValue
jsonBool = (JsonBool True <$ stringP "true") <|> (JsonBool False <$ stringP "false")

jsonNumber :: Parser JsonValue
jsonNumber = do
    s <- spanP (\c -> isDigit c || c == '.' || c == '-')
    if null s then empty else return $ JsonNumber (read s)

jsonString :: Parser JsonValue
jsonString = JsonString <$> (charP '"' *> spanP (/= '"') <* charP '"')

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (charP '[' *> ws *> elements <* ws <* charP ']')
  where
    elements = sepBy (ws *> jsonValue <* ws) (charP ',')

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (charP '{' *> ws *> pairs <* ws <* charP '}')
  where
    pairs = sepBy (ws *> pair <* ws) (charP ',')
    pair = do
        JsonString key <- jsonString
        ws *> charP ':' *> ws
        val <- jsonValue
        return (key, val)

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = (:) <$> p <*> many (sep *> p) <|> pure []

parseJson :: String -> Maybe JsonValue
parseJson s = fst <$> runParser (ws *> jsonValue <* ws) s
