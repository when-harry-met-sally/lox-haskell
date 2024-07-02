import Data.Char (isDigit)

data TokenType
  = -- Single char tokens
    LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACE
  | RIGHT_BRACE
  | COMMA
  | DOT
  | MINUS
  | PLUS
  | SEMICOLON
  | SLASH
  | STAR
  | -- Non single
    BANG
  | BANG_EQUAL
  | EQUAL
  | EQUAL_EQUAL
  | GREATER
  | GREATER_EQUAL
  | LESS
  | LESS_EQUAL
  | -- Literals
    IDENTIFIER
  | STRING
  | NUMBER
  | -- Keywords
    AND
  | CLASS
  | ELSE
  | FALSE
  | FUN
  | FOR
  | IF
  | NIL
  | OR
  | PRINT
  | RETURN
  | SUPER
  | THIS
  | TRUE
  | VAR
  | WHILE
  | EOF
  deriving (Show, Eq)

addToken :: TokenType -> [Token] -> Maybe String -> Int -> [Token]
addToken tokenType tokens literal line = (t : tokens)
  where
    t = Token tokenType "lexeme" literal line

data Token = Token {tokenType :: TokenType, lexeme :: String, literal :: Maybe String, line :: Int}

instance Show Token where
  show (Token tokenType lexeme literal _) =
    show tokenType ++ formatLiteral literal
    where
      formatLiteral :: Maybe String -> String
      formatLiteral = maybe "" (\lit -> " (" ++ lit ++ ")")

-- scan :: [String] -> [Token]
-- scan (x : xs) = go x
--   where
--     go :: String -> [Token]
--     go line = matchToken line

getToken :: String -> Int -> Maybe Token
getToken input@(x : xs) l
  | otherwise = match input l

match :: String -> Int -> Maybe Token
-- Special
match ('"' : xs) l = Just (Token STRING "" (Just literal) l)
  where
    (literal, rest) = quoteLookAhead xs []
match ('/' : '/' : _) l = Nothing
-- Multichars
match ('!' : '=' : xs) l = Just (Token BANG_EQUAL "" Nothing l)
match ('=' : '=' : xs) l = Just (Token EQUAL_EQUAL "" Nothing l)
match ('<' : '=' : xs) l = Just (Token LESS_EQUAL "" Nothing l)
match ('>' : '=' : xs) l = Just (Token GREATER_EQUAL "" Nothing l)
-- Single chars
match ('!' : xs) l = Just (Token BANG "" Nothing l)
match ('=' : xs) l = Just (Token EQUAL "" Nothing l)
match ('<' : xs) l = Just (Token LESS "" Nothing l)
match ('>' : xs) l = Just (Token GREATER "" Nothing l)
--
match ('/' : xs) l = Just (Token SLASH "" Nothing l)
match ('(' : xs) l = Just (Token LEFT_PAREN "" Nothing l)
match (')' : xs) l = Just (Token RIGHT_PAREN "" Nothing l)
match ('{' : xs) l = Just (Token LEFT_BRACE "" Nothing l)
match ('}' : xs) l = Just (Token RIGHT_BRACE "" Nothing l)
match (',' : xs) l = Just (Token COMMA "" Nothing l)
match ('.' : xs) l = Just (Token DOT "" Nothing l)
match ('-' : xs) l = Just (Token MINUS "" Nothing l)
match ('+' : xs) l = Just (Token PLUS "" Nothing l)
match (';' : xs) l = Just (Token SEMICOLON "" Nothing l)
match ('*' : xs) l = Just (Token STAR "" Nothing l)
-- Fallback
match (_ : xs) l = error "Unknown character" -- match xs tokens l

main :: IO ()
main = do
  content <- readFile "test.lox"
  print "----"
  print "File Content"
  print content
  let tokens = scan $ lines content
  print "----"
  print "Tokens"
  print tokens

-- TODO:
-- For tomorrow, we simply need to seperate "match" and the matching for the other functions. If not alphanumeric, we call "match".

quoteLookAhead :: String -> String -> (String, String)
quoteLookAhead [] _ = error "Quotes not terminated"
quoteLookAhead ('\\' : '"' : xs) acc = quoteLookAhead xs (acc ++ "\\\"")
quoteLookAhead ('"' : xs) acc = (acc, xs)
quoteLookAhead (x : xs) acc = quoteLookAhead xs (acc ++ [x])
