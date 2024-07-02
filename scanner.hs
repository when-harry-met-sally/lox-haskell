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

addToken :: TokenType -> [Token] -> [Token]
addToken tokenType tokens = (t : tokens)
  where
    t = Token tokenType "a" "a" 3

data Token = Token {tokenType :: TokenType, lexeme :: String, literal :: String, line :: Int}

instance Show Token where -- Todo, add tokenType to show
  show (Token tokenType lexeme literal _) = show tokenType

scan :: String -> [Token]
scan content = reverse $ foldl (\acc line -> get line acc) [] (lines content)

quoteLookAhead :: String -> String -> (String, String)
quoteLookAhead [] _ = error "Quotes not terminated"
quoteLookAhead ('\\' : '"' : xs) acc = quoteLookAhead xs (acc ++ "\\\"")
quoteLookAhead ('"' : xs) acc = (acc, xs)
quoteLookAhead (x : xs) acc = quoteLookAhead xs (acc ++ xs)

get :: String -> [Token] -> [Token]
get [] tokens = tokens
get ('"' : xs) tokens = get rest (addToken STRING tokens)
  where
    (literal, rest) = quoteLookAhead xs []
get ('=' : '=' : xs) tokens = get xs (addToken EQUAL_EQUAL tokens)
get ('(' : xs) tokens = get xs (addToken LEFT_PAREN tokens)
get (_ : xs) tokens = get xs tokens

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
