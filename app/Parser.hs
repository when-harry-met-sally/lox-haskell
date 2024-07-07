module Parser (parse) where

import Shared

parseFactor :: [Token] -> (Expression, [Token])
parseFactor (Token tokenType _ literal l : rest) = case tokenType of
  MINUS ->
    let (expr, rest') = parseFactor rest
     in (Negate expr, rest')
  BANG ->
    let (expr, rest') = parseFactor rest
     in (Not expr, rest')
  LEFT_PAREN ->
    let (expr, rest') = parseExpression rest
     in case rest' of
          (Token RIGHT_PAREN _ _ l : rest'') -> (Grouping expr, rest'')
          _ -> error "No closing parenthesis" -- Add line number
  NUMBER -> case literal of
    Just x -> (Number (read x), rest)
    Nothing -> error "Invalid number format"
  TRUE -> (Boolean True, rest)
  FALSE -> (Boolean False, rest)
  t -> error ("Invalid factor token: " ++ show t)

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

parseComparison :: [Token] -> (Expression, [Token])
parseComparison tokens =
  let (term, rest) = parseExpression tokens
   in case rest of
        (Token EQUAL_EQUAL _ _ _ : rest') ->
          let (expr, rest'') = parseComparison rest'
           in (Equal term expr, rest'')
        (Token BANG_EQUAL _ _ _ : rest') ->
          let (expr, rest'') = parseComparison rest'
           in (NotEqual term expr, rest'')
        (Token LESS _ _ _ : rest') ->
          let (expr, rest'') = parseComparison rest'
           in (Less term expr, rest'')
        (Token LESS_EQUAL _ _ _ : rest') ->
          let (expr, rest'') = parseComparison rest'
           in (LessEqual term expr, rest'')
        (Token GREATER _ _ _ : rest') ->
          let (expr, rest'') = parseComparison rest'
           in (Greater term expr, rest'')
        (Token GREATER_EQUAL _ _ _ : rest') ->
          let (expr, rest'') = parseComparison rest'
           in (GreaterEqual term expr, rest'')
        _ -> (term, rest)

parseStatement :: [Token] -> (Statement, [Token])
parseStatement tokens =
  case tokens of
    (Token PRINT _ _ _ : rest) ->
      let (expr, rest') = parseComparison rest
       in case rest' of
            (Token SEMICOLON _ _ _ : rest'') -> (PrintStatement expr, rest'')
            _ -> error "Expected semicolon after print statement"
    _ ->
      let (comp, rest) = parseComparison tokens
       in case rest of
            (Token SEMICOLON _ _ _ : rest') -> (ExpressionStatement comp, rest')
            _ -> error "Expected semicolon after statement"

parseDeclaration :: [Token] -> (Declaration, [Token])
parseDeclaration tokens =
  case tokens of
    c@(Token VAR _ _ _ : rest) -> case c of
      (Token VAR _ _ _ : Token IDENTIFIER name _ _ : Token EQUAL _ _ _ : Token STRING _ val _ : Token SEMICOLON _ _ _ : rest') ->
        case val of
          (Just literal) -> (VarDeclaration (Identifier name) (Str literal), rest')
          _ -> error "Invalid var declaration syntax"
    _ ->
      let (stmt, rest) = parseStatement tokens
       in (StatementDeclaration stmt, rest)

parseProgram :: [Token] -> (Program, [Token])
parseProgram tokens =
  let parseAll decs tokens =
        let (dec, rest) = parseDeclaration tokens
         in case rest of
              (Token EOF _ _ _ : rest') -> (Program (decs ++ [dec]), rest')
              _ -> parseAll (decs ++ [dec]) rest
   in parseAll [] tokens

parse :: [Token] -> Program
parse tokens = fst $ parseProgram tokens
