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

scan :: String -> [Token]
scan content =
  reverse $
    fst $
      foldl
        ( \(tokens, lineNumber) line ->
            (get line tokens lineNumber, lineNumber + 1)
        )
        ([], 0)
        (lines content)

getToken :: String -> [Token] -> Int -> [Token]
getToken input@(x : xs) tokens l
  | otherwise = get input tokens l

get :: String -> [Token] -> Int -> [Token]
get [] tokens l = tokens
-- Special
get ('"' : xs) tokens l = get rest (addToken STRING tokens (Just literal) l) l
  where
    (literal, rest) = quoteLookAhead xs []
get ('/' : '/' : _) tokens l = tokens
-- Multichars
get ('!' : '=' : xs) tokens l = get xs (addToken BANG_EQUAL tokens Nothing l) l
get ('=' : '=' : xs) tokens l = get xs (addToken EQUAL_EQUAL tokens Nothing l) l
get ('<' : '=' : xs) tokens l = get xs (addToken LESS_EQUAL tokens Nothing l) l
get ('>' : '=' : xs) tokens l = get xs (addToken GREATER_EQUAL tokens Nothing l) l
-- Single chars
get ('!' : xs) tokens l = get xs (addToken BANG tokens Nothing l) l
get ('=' : xs) tokens l = get xs (addToken EQUAL tokens Nothing l) l
get ('<' : xs) tokens l = get xs (addToken LESS tokens Nothing l) l
get ('>' : xs) tokens l = get xs (addToken GREATER tokens Nothing l) l
--
get ('/' : xs) tokens l = get xs (addToken SLASH tokens Nothing l) l
get ('(' : xs) tokens l = get xs (addToken LEFT_PAREN tokens Nothing l) l
get (')' : xs) tokens l = get xs (addToken RIGHT_PAREN tokens Nothing l) l
get ('{' : xs) tokens l = get xs (addToken LEFT_BRACE tokens Nothing l) l
get ('}' : xs) tokens l = get xs (addToken RIGHT_BRACE tokens Nothing l) l
get (',' : xs) tokens l = get xs (addToken COMMA tokens Nothing l) l
get ('.' : xs) tokens l = get xs (addToken DOT tokens Nothing l) l
get ('-' : xs) tokens l = get xs (addToken MINUS tokens Nothing l) l
get ('+' : xs) tokens l = get xs (addToken PLUS tokens Nothing l) l
get (';' : xs) tokens l = get xs (addToken SEMICOLON tokens Nothing l) l
get ('*' : xs) tokens l = get xs (addToken STAR tokens Nothing l) l
-- Fallback
get (_ : xs) tokens l = error "Unknown character" -- get xs tokens l

main :: IO ()
main = do
  content <- readFile "test.lox"
  print "----"
  print "File Content"
  print content
  let tokens = scan content
  print "----"
  print "Tokens"
  print tokens

-- TODO:
-- For tomorrow, we simply need to seperate "get" and the matching for the other functions. If not alphanumeric, we call "get".

quoteLookAhead :: String -> String -> (String, String)
quoteLookAhead [] _ = error "Quotes not terminated"
quoteLookAhead ('\\' : '"' : xs) acc = quoteLookAhead xs (acc ++ "\\\"")
quoteLookAhead ('"' : xs) acc = (acc, xs)
quoteLookAhead (x : xs) acc = quoteLookAhead xs (acc ++ [x])
