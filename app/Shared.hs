module Shared (Token (..), TokenType (..), Expression (..), Value (..), Statement (..), Program (..), Declaration (..)) where

data Value = IntVal Int | BoolVal Bool | StringVal String | NilValue
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
  | -- Multi
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
  | Str String
  | Nil
  | Not Expression
  | Identifier String
  | Exponent Expression Expression
  | Boolean Bool
  | -- Logical operators
    And Expression Expression
  | Or Expression Expression
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
  | Assignment String Expression
  | Statement Expression
  deriving (Show, Eq)

-- FALSY is nil or False

newtype Program = Program [Declaration]
  deriving (Show, Eq)

data Declaration
  = VarDeclaration String Expression
  | FunctionDeclaration String [Expression] [Declaration]
  | StatementDeclaration Statement
  deriving (Show, Eq)

data Statement
  = ExpressionStatement Expression
  | PrintStatement Expression
  | Block [Declaration]
  | WhileStatement Expression [Declaration]
  | ForStatement Expression [Declaration]
  | IfStatement Expression [Declaration]
  | IfElseStatement Expression [Declaration] [Declaration]
  deriving (Show, Eq)

data Token = Token {tokenType :: TokenType, lexeme :: String, literal :: Maybe String, line :: Int}
  deriving (Eq) -- Add Eq here

instance Show Token where
  show (Token tokenType lexeme literal _) =
    show tokenType ++ "[" ++ lexeme ++ "]" ++ formatLiteral literal
    where
      formatLiteral :: Maybe String -> String
      formatLiteral = maybe "" (\lit -> " (" ++ lit ++ ")")
