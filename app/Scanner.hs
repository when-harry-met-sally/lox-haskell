module Scanner (TokenType (..), scan) where

import Data.Char (isAlpha, isDigit)
import Shared

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

getEOF :: Int -> Token
getEOF = Token EOF "" Nothing

scan :: String -> [Token]
scan text = go text text 1 []
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
      Just x -> case aLiteral of
        "true" -> (Just (Token TRUE aLiteral Nothing l), aRest, l)
        "false" -> (Just (Token FALSE aLiteral Nothing l), aRest, l)
        _ -> (Just (Token x aLiteral Nothing l), aRest, l)
      Nothing -> (Just (Token IDENTIFIER aLiteral Nothing l), aRest, l)
  | otherwise = match input l
  where
    (nLiteral, nRest) = digitLookAhead xs [x] False
    (aLiteral, aRest) = alphaLookAhead xs [x]
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
  ('*' : '*' : xs) -> (Just (Token STAR_STAR "**" Nothing l), xs, l)
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
digitLookAhead c@(')' : xs) acc p = (reverse acc, c)
digitLookAhead (' ' : xs) acc p = (reverse acc, xs)
digitLookAhead ('\n' : xs) acc _ = (reverse acc, xs)
digitLookAhead ('.' : y : xs) acc p
  | isDigit y && not p = digitLookAhead xs (y : '.' : acc) True
  | otherwise = error "Bad period"
digitLookAhead c@(x : xs) acc p
  | isDigit x = digitLookAhead xs (x : acc) p
  | isAlpha x = error ("Bad symbol (" ++ [x] ++ ")")
  | otherwise = (reverse acc, c)

alphaLookAhead :: String -> String -> (String, String)
alphaLookAhead [] acc = (reverse acc, [])
alphaLookAhead (' ' : xs) acc = (reverse acc, xs)
alphaLookAhead ('\n' : xs) acc = (reverse acc, xs)
alphaLookAhead c@(x : xs) acc
  | isValidString x = alphaLookAhead xs (x : acc)
  | isDigit x = alphaLookAhead xs (x : acc)
  | otherwise = (reverse acc, c)
