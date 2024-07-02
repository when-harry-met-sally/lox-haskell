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
parseKeyword str = case str of
  "and" -> Just AND
  "class" -> Just CLASS
  "else" -> Just ELSE
  "false" -> Just FALSE
  "fun" -> Just FUN
  "for" -> Just FOR
  "if" -> Just IF
  "nil" -> Just NIL
  "or" -> Just OR
  "print" -> Just PRINT
  "return" -> Just RETURN
  "super" -> Just SUPER
  "this" -> Just THIS
  "true" -> Just TRUE
  "var" -> Just VAR
  "while" -> Just WHILE
  _ -> Nothing

data Token = Token {tokenType :: TokenType, lexeme :: String, literal :: Maybe String, line :: Int}

instance Show Token where
  show (Token tokenType lexeme literal _) =
    show tokenType ++ "(" ++ lexeme ++ ")" ++ formatLiteral literal
    where
      formatLiteral :: Maybe String -> String
      formatLiteral = maybe "" (\lit -> " (" ++ lit ++ ")")

scan :: [String] -> [Token] -> [Token]
scan [] tokens = tokens
scan (x : xs) tokens = (go x tokens ++ scan xs tokens)
  where
    go :: String -> [Token] -> [Token]
    go [] tokens = reverse tokens
    go line tokens = case token of
      Nothing -> go rest tokens
      Just token -> go rest (token : tokens)
      where
        (token, rest) = getToken line 0

isValidString :: Char -> Bool
isValidString c = isAlpha c || c == '_'

getToken :: String -> Int -> (Maybe Token, String)
getToken input@(x : xs) l
  | isDigit x = (Just (Token NUMBER nLiteral (Just nLiteral) l), nRest)
  | isValidString x = case keyword of
      Just x -> (Just (Token x aLiteral Nothing l), aRest)
      Nothing -> (Just (Token IDENTIFIER aLiteral (Just aLiteral) l), aRest)
  | otherwise = match input l
  where
    (nLiteral, nRest) = digitLookAhead xs [x] False
    (aLiteral, aRest) = alphaLookAhead xs [x] False
    keyword = parseKeyword aLiteral

match :: String -> Int -> (Maybe Token, String)
match str l = case str of
  -- Special
  ('"' : xs) ->
    let (literal, rest) = quoteLookAhead xs []
     in (Just (Token STRING ("\"" ++ literal ++ "\"") (Just literal) l), rest)
  ('/' : '/' : _) -> (Nothing, [])
  -- Multichars
  ('!' : '=' : xs) -> (Just (Token BANG_EQUAL "!=" Nothing l), xs)
  ('=' : '=' : xs) -> (Just (Token EQUAL_EQUAL "==" Nothing l), xs)
  ('<' : '=' : xs) -> (Just (Token LESS_EQUAL "<=" Nothing l), xs)
  ('>' : '=' : xs) -> (Just (Token GREATER_EQUAL ">=" Nothing l), xs)
  -- Single chars
  (c@'!' : xs) -> (Just (Token BANG [c] Nothing l), xs)
  (c@'=' : xs) -> (Just (Token EQUAL [c] Nothing l), xs)
  (c@'<' : xs) -> (Just (Token LESS [c] Nothing l), xs)
  (c@'>' : xs) -> (Just (Token GREATER [c] Nothing l), xs)
  (c@'/' : xs) -> (Just (Token SLASH [c] Nothing l), xs)
  (c@'(' : xs) -> (Just (Token LEFT_PAREN [c] Nothing l), xs)
  (c@')' : xs) -> (Just (Token RIGHT_PAREN [c] Nothing l), xs)
  (c@'{' : xs) -> (Just (Token LEFT_BRACE [c] Nothing l), xs)
  (c@'}' : xs) -> (Just (Token RIGHT_BRACE [c] Nothing l), xs)
  (c@',' : xs) -> (Just (Token COMMA [c] Nothing l), xs)
  (c@'.' : xs) -> (Just (Token DOT [c] Nothing l), xs)
  (c@'-' : xs) -> (Just (Token MINUS [c] Nothing l), xs)
  (c@'+' : xs) -> (Just (Token PLUS [c] Nothing l), xs)
  (c@';' : xs) -> (Just (Token SEMICOLON [c] Nothing l), xs)
  (c@'*' : xs) -> (Just (Token STAR [c] Nothing l), xs)
  -- Fallback. TODO: Ignore white space specifically
  (_ : xs) -> (Nothing, xs)

quoteLookAhead :: String -> String -> (String, String)
quoteLookAhead [] _ = error "Quotes not terminated"
quoteLookAhead ('\\' : '"' : xs) acc = quoteLookAhead xs (acc ++ "\\\"")
quoteLookAhead ('"' : xs) acc = (acc, xs)
quoteLookAhead (x : xs) acc = quoteLookAhead xs (acc ++ [x])

digitLookAhead :: String -> String -> Bool -> (String, String)
digitLookAhead [] acc _ = (reverse acc, [])
digitLookAhead (' ' : xs) acc p = (acc, xs)
digitLookAhead ('.' : y : xs) acc p
  | isDigit y && p == False = digitLookAhead xs (y : '.' : acc) True
  | otherwise = error "Bad period"
digitLookAhead (x : xs) acc p
  | isDigit x = digitLookAhead xs (x : acc) p
  | otherwise = error "Bad symbol"

alphaLookAhead :: String -> String -> Bool -> (String, String)
alphaLookAhead [] acc _ = (reverse acc, [])
alphaLookAhead (' ' : xs) acc p = (reverse acc, xs)
alphaLookAhead (x : xs) acc p
  | isValidString x = alphaLookAhead xs (x : acc) p
  | isDigit x = alphaLookAhead xs (x : acc) p
  | otherwise = error "Bad symbol"

main :: IO ()
main = do
  content <- readFile "test.lox"
  print "----"
  print "File Content"
  print content
  let tokens = scan (lines content) []
  print "----"
  print "Tokens"
  print tokens
