module Scanner (Token (..), TokenType (..)) where

import Data.Char (isAlpha, isDigit)

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

parseKeyword :: String -> Maybe TokenType
parseKeyword str = lookup str keywords
  where
    keywords =
      [ ("and", AND),
        ("class", CLASS),
        ("else", ELSE),
        ("false", FALSE),
        ("fun", FUN),
        ("for", FOR),
        ("if", IF),
        ("nil", NIL),
        ("or", OR),
        ("print", PRINT),
        ("return", RETURN),
        ("super", SUPER),
        ("this", THIS),
        ("true", TRUE),
        ("var", VAR),
        ("while", WHILE)
      ]

data Token = Token {tokenType :: TokenType, lexeme :: String, literal :: Maybe String, line :: Int}

instance Show Token where
  show (Token tokenType lexeme literal _) =
    show tokenType ++ "[" ++ lexeme ++ "]" ++ formatLiteral literal
    where
      formatLiteral :: Maybe String -> String
      formatLiteral = maybe "" (\lit -> " (" ++ lit ++ ")")

getEOF :: Int -> Token
getEOF = Token EOF "" Nothing

scan :: String -> [Token]
scan text = go text text 0 []
  where
    go :: String -> String -> Int -> [Token] -> [Token]
    go _ [] line tokens = reverse (getEOF line : tokens)
    go text r line tokens = case token of
      Just t -> go text rest line (t : tokens)
      _ -> go text rest line tokens
      where
        (token, rest, l) = getToken r line

isValidString :: Char -> Bool
isValidString c = isAlpha c || c == '_'

getToken :: String -> Int -> (Maybe Token, String, Int)
getToken [] l = (Nothing, [], l)
getToken input@(x : xs) l
  | isDigit x = (Just (Token NUMBER nLiteral (Just nLiteral) l), nRest, l)
  | isValidString x = case keyword of
      Just x -> (Just (Token x aLiteral Nothing l), aRest, l)
      Nothing -> (Just (Token IDENTIFIER aLiteral Nothing l), aRest, l)
  | otherwise = match input l
  where
    (nLiteral, nRest) = digitLookAhead xs [x] False
    (aLiteral, aRest) = alphaLookAhead xs [x] False
    keyword = parseKeyword aLiteral

match :: String -> Int -> (Maybe Token, String, Int)
match str l = case str of
  -- Special
  ('"' : xs) ->
    let (literal, rest) = quoteLookAhead xs []
     in (Just (Token STRING ("\"" ++ literal ++ "\"") (Just literal) l), rest, l)
  ('/' : '/' : xs) ->
    let (newLineNumber, rest) = getNextNewLine l xs
     in (Nothing, rest, newLineNumber)
  ('\n' : xs) -> (Nothing, xs, l + 1)
  -- Multichars
  ('!' : '=' : xs) -> (Just (Token BANG_EQUAL "!=" Nothing l), xs, l)
  ('=' : '=' : xs) -> (Just (Token EQUAL_EQUAL "==" Nothing l), xs, l)
  ('<' : '=' : xs) -> (Just (Token LESS_EQUAL "<=" Nothing l), xs, l)
  ('>' : '=' : xs) -> (Just (Token GREATER_EQUAL ">=" Nothing l), xs, l)
  -- Single chars
  (c@'!' : xs) -> (Just (Token BANG [c] Nothing l), xs, l)
  (c@'=' : xs) -> (Just (Token EQUAL [c] Nothing l), xs, l)
  (c@'<' : xs) -> (Just (Token LESS [c] Nothing l), xs, l)
  (c@'>' : xs) -> (Just (Token GREATER [c] Nothing l), xs, l)
  (c@'/' : xs) -> (Just (Token SLASH [c] Nothing l), xs, l)
  (c@'(' : xs) -> (Just (Token LEFT_PAREN [c] Nothing l), xs, l)
  (c@')' : xs) -> (Just (Token RIGHT_PAREN [c] Nothing l), xs, l)
  (c@'{' : xs) -> (Just (Token LEFT_BRACE [c] Nothing l), xs, l)
  (c@'}' : xs) -> (Just (Token RIGHT_BRACE [c] Nothing l), xs, l)
  (c@',' : xs) -> (Just (Token COMMA [c] Nothing l), xs, l)
  (c@'.' : xs) -> (Just (Token DOT [c] Nothing l), xs, l)
  (c@'-' : xs) -> (Just (Token MINUS [c] Nothing l), xs, l)
  (c@'+' : xs) -> (Just (Token PLUS [c] Nothing l), xs, l)
  (c@';' : xs) -> (Just (Token SEMICOLON [c] Nothing l), xs, l)
  (c@'*' : xs) -> (Just (Token STAR [c] Nothing l), xs, l)
  (_ : xs) -> (Nothing, xs, l)

quoteLookAhead :: String -> String -> (String, String)
quoteLookAhead [] _ = error "Unterminated quotation"
quoteLookAhead ('"' : xs) acc = (reverse acc, xs)
quoteLookAhead ('\\' : c : xs) acc = quoteLookAhead xs (c : acc)
quoteLookAhead (x : xs) acc = quoteLookAhead xs (x : acc)

getNextNewLine :: Int -> String -> (Int, String)
getNextNewLine l [] = (l, [])
getNextNewLine l ('\n' : xs) = (l + 1, xs)
getNextNewLine l (_ : xs) = getNextNewLine l xs

digitLookAhead :: String -> String -> Bool -> (String, String)
digitLookAhead [] acc _ = (reverse acc, [])
digitLookAhead (' ' : xs) acc p = (reverse acc, xs)
digitLookAhead ('\n' : xs) acc p = (reverse acc, xs)
digitLookAhead ('.' : y : xs) acc p
  | isDigit y && p == False = digitLookAhead xs (y : '.' : acc) True
  | otherwise = error "Bad period"
digitLookAhead (x : xs) acc p
  | isDigit x = digitLookAhead xs (x : acc) p
  | otherwise = error ("Bad symbol (" ++ [x] ++ ")")

alphaLookAhead :: String -> String -> Bool -> (String, String)
alphaLookAhead [] acc _ = (reverse acc, [])
alphaLookAhead (' ' : xs) acc p = (reverse acc, xs)
alphaLookAhead ('\n' : xs) acc p = (reverse acc, xs)
alphaLookAhead (x : xs) acc p
  | isValidString x = alphaLookAhead xs (x : acc) p
  | isDigit x = alphaLookAhead xs (x : acc) p
  | otherwise = error ("Bad symbol (" ++ [x] ++ ")")

main :: IO ()
main = do
  content <- readFile "../test/lox/scanner.lox"
  print "----"
  print "File Content"
  print content
  let tokens = scan content
  print "----"
  print "Tokens"
  print tokens
