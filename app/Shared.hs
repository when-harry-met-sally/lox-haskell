module Shared (Token (..), TokenType (..), Expression (..), Value (..)) where

data Value = IntVal Int | BoolVal Bool | StringVal String
  deriving (Show, Eq, Ord)

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
  | STAR_STAR
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

data Expression
  = -- Factor
    Grouping Expression
  | Negate Expression
  | Number Int
  | Not Expression
  | Exponent Expression Expression
  | Boolean Bool
  | -- Term
    Add Expression Expression
  | Subtract Expression Expression
  | --
    Multiply Expression Expression
  | Divide Expression Expression
  | -- Comparison
    Greater Expression Expression
  | GreaterEqual Expression Expression
  | Less Expression Expression
  | LessEqual Expression Expression
  | Equal Expression Expression
  | NotEqual Expression Expression
  deriving (Show, Eq)

data Token = Token {tokenType :: TokenType, lexeme :: String, literal :: Maybe String, line :: Int}
  deriving (Eq) -- Add Eq here

instance Show Token where
  show (Token tokenType lexeme literal _) =
    show tokenType ++ "[" ++ lexeme ++ "]" ++ formatLiteral literal
    where
      formatLiteral :: Maybe String -> String
      formatLiteral = maybe "" (\lit -> " (" ++ lit ++ ")")
