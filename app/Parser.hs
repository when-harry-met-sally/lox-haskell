module Parser (parse) where

import Shared

parseFactor :: [Token] -> (Expression, [Token])
parseFactor (Token tokenType _ literal l : rest) = case tokenType of
  MINUS ->
    let (expr, rest') = parseFactor rest
     in (Negate expr, rest')
  LEFT_PAREN ->
    let (expr, rest') = parseExpression rest
     in case rest' of
          (Token RIGHT_PAREN _ _ l : rest'') -> (Grouping expr, rest'')
          _ -> error "No closing parenthesis" -- Add line number
  NUMBER -> case literal of
    Just x -> (Number (read x), rest)
    Nothing -> error "Invalid number format"
  _ -> error "Invalid token"

parseTerm :: [Token] -> (Expression, [Token])
parseTerm tokens =
  let (fact, rest) = parseFactor tokens
   in case rest of
        (Token STAR _ _ _ : rest') ->
          let (expr, rest'') = parseTerm rest'
           in (Multiply fact expr, rest'')
        (Token SLASH _ _ _ : rest') ->
          let (expr, rest'') = parseTerm rest'
           in (Divide fact expr, rest'')
        _ -> (fact, rest)

parseExpression :: [Token] -> (Expression, [Token])
parseExpression tokens =
  let (term, rest) = parseTerm tokens
   in case rest of
        (Token STAR_STAR _ _ _ : rest') ->
          let (expr, rest'') = parseExpression rest'
           in (Exponent term expr, rest'')
        (Token PLUS _ _ _ : rest') ->
          let (expr, rest'') = parseExpression rest'
           in (Add term expr, rest'')
        (Token MINUS _ _ _ : rest') ->
          let (expr, rest'') = parseExpression rest'
           in (Subtract term expr, rest'')
        _ -> (term, rest)

parse :: [Token] -> Expression
parse tokens = fst $ parseExpression tokens
