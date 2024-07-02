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

data Token = Token {tokenType :: TokenType, lexeme :: String, literal :: String, line :: Int}

instance Show Token where -- Todo, add tokenType to show
  show (Token _ lexeme literal _) = lexeme ++ literal

scan :: String -> [Token]
scan content = go $ lines content
  where
    go :: [String] -> [Token]
    go content = []
      where
        endOfLine = False

main :: IO ()
main = do
  content <- readFile "test.lox"
  print "File Content"
  print content
  let tokens = scan content
  print "Tokens"
  print tokens
