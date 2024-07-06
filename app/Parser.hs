module Parser (parse) where

import Shared

parseFactor :: [Token] -> (Expression, [Token])
parseFactor (Token tokenType _ literal _ : rest) = case tokenType of
  LEFT_PAREN ->
    let (expr, rest') = parseExpression rest
     in case rest' of
          (Token RIGHT_PAREN _ _ _ : rest'') -> (Grouping expr, rest'')
          _ -> error "Bad"
  NUMBER -> case literal of
    Just x -> (Number (read x), rest)
    Nothing -> error "Invalid number format"
  _ -> error "Invalid token"

parseTerm :: [Token] -> (Expression, [Token])
parseTerm tokens =
  let (term, rest) = parseFactor tokens
   in case rest of
        (Token STAR _ _ _ : rest') ->
          let (expr, rest'') = parseTerm rest'
           in (Multiply term expr, rest'')
        (Token SLASH _ _ _ : rest') ->
          let (expr, rest'') = parseTerm rest'
           in (Divide term expr, rest'')
        _ -> (term, rest)

parseExpression :: [Token] -> (Expression, [Token])
parseExpression tokens =
  let (term, rest) = parseTerm tokens
   in case rest of
        (Token PLUS _ _ _ : rest') ->
          let (expr, rest'') = parseExpression rest'
           in (Add term expr, rest'')
        (Token MINUS _ _ _ : rest') ->
          let (expr, rest'') = parseExpression rest'
           in (Subtract term expr, rest'')
        _ -> (term, rest)

parse :: [Token] -> Expression
parse tokens = fst $ parseExpression tokens
