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

addToken :: TokenType -> [Token] -> Int -> [Token]
addToken tokenType tokens line = (t : tokens)
  where
    t = Token tokenType "a" "a" line

data Token = Token {tokenType :: TokenType, lexeme :: String, literal :: String, line :: Int}

instance Show Token where -- Todo, add tokenType to show
  show (Token tokenType lexeme literal _) = show tokenType

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

quoteLookAhead :: String -> String -> (String, String)
quoteLookAhead [] _ = error "Quotes not terminated"
quoteLookAhead ('\\' : '"' : xs) acc = quoteLookAhead xs (acc ++ "\\\"")
quoteLookAhead ('"' : xs) acc = (acc, xs)
quoteLookAhead (x : xs) acc = quoteLookAhead xs (acc ++ xs)

get :: String -> [Token] -> Int -> [Token]
get [] tokens l = tokens
get ('"' : xs) tokens l = get rest (addToken STRING tokens l) l
  where
    (literal, rest) = quoteLookAhead xs []
get ('=' : '=' : xs) tokens l = get xs (addToken EQUAL_EQUAL tokens l) l
get ('(' : xs) tokens l = get xs (addToken LEFT_PAREN tokens l) l
get (_ : xs) tokens l = get xs tokens l

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
