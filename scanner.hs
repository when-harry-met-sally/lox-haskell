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
    show tokenType ++ formatLiteral literal
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
  | isDigit x = (Just (Token NUMBER "" (Just nLiteral) l), nRest)
  | isValidString x = case keyword of
      Just x -> (Just (Token x "" Nothing l), aRest)
      Nothing -> (Just (Token IDENTIFIER "" (Just aLiteral) l), aRest)
  | otherwise = match input l
  where
    (nLiteral, nRest) = digitLookAhead xs [x] False
    (aLiteral, aRest) = alphaLookAhead xs [x] False
    keyword = parseKeyword aLiteral

match :: String -> Int -> (Maybe Token, String)
-- Special
match ('"' : xs) l = (Just (Token STRING "" (Just literal) l), rest)
  where
    (literal, rest) = quoteLookAhead xs []
match ('/' : '/' : _) l = (Nothing, [])
-- Multichars
match ('!' : '=' : xs) l = (Just (Token BANG_EQUAL "" Nothing l), xs)
match ('=' : '=' : xs) l = (Just (Token EQUAL_EQUAL "" Nothing l), xs)
match ('<' : '=' : xs) l = (Just (Token LESS_EQUAL "" Nothing l), xs)
match ('>' : '=' : xs) l = (Just (Token GREATER_EQUAL "" Nothing l), xs)
-- Single chars
match ('!' : xs) l = (Just (Token BANG "" Nothing l), xs)
match ('=' : xs) l = (Just (Token EQUAL "" Nothing l), xs)
match ('<' : xs) l = (Just (Token LESS "" Nothing l), xs)
match ('>' : xs) l = (Just (Token GREATER "" Nothing l), xs)
--
match ('/' : xs) l = (Just (Token SLASH "" Nothing l), xs)
match ('(' : xs) l = (Just (Token LEFT_PAREN "" Nothing l), xs)
match (')' : xs) l = (Just (Token RIGHT_PAREN "" Nothing l), xs)
match ('{' : xs) l = (Just (Token LEFT_BRACE "" Nothing l), xs)
match ('}' : xs) l = (Just (Token RIGHT_BRACE "" Nothing l), xs)
match (',' : xs) l = (Just (Token COMMA "" Nothing l), xs)
match ('.' : xs) l = (Just (Token DOT "" Nothing l), xs)
match ('-' : xs) l = (Just (Token MINUS "" Nothing l), xs)
match ('+' : xs) l = (Just (Token PLUS "" Nothing l), xs)
match (';' : xs) l = (Just (Token SEMICOLON "" Nothing l), xs)
match ('*' : xs) l = (Just (Token STAR "" Nothing l), xs)
-- Fallback. TODO: Ignore white space specifically
match (_ : xs) l = (Nothing, xs)

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
