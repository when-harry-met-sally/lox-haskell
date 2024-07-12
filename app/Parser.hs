module Parser (parse) where

import Debug.Trace (trace)
import Shared

parseFactor :: [Token] -> (Expression, [Token])
parseFactor (Token tokenType lexeme literal l : rest) = case tokenType of
  MINUS ->
    let (expr, rest') = parseFactor rest
     in (Negate expr, rest')
  BANG ->
    let (expr, rest') = parseFactor rest
     in (Not expr, rest')
  LEFT_PAREN ->
    let (expr, rest') = parseLogicalOperators rest
     in case rest' of
          (Token RIGHT_PAREN _ _ l : rest'') -> (Grouping expr, rest'')
          _ -> error "No closing parenthesis" -- Add line number
  NUMBER -> case literal of
    Just x -> (Number (read x), rest)
    Nothing -> error "Invalid number format"
  STRING -> case literal of
    Just x -> (Str x, rest)
    Nothing -> error "Invalid number format"
  NIL -> (Nil, rest)
  TRUE -> (Boolean True, rest)
  FALSE -> (Boolean False, rest)
  IDENTIFIER -> (Identifier lexeme, rest)
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

parseLogicalOperators :: [Token] -> (Expression, [Token])
parseLogicalOperators tokens =
  let (expr, rest) = parseComparison tokens
   in case rest of
        (Token OR _ _ _ : rest') ->
          let (expr2, rest'') = parseLogicalOperators rest'
           in (Or expr expr2, rest'')
        (Token AND _ _ _ : rest') ->
          let (expr2, rest'') = parseLogicalOperators rest'
           in (And expr expr2, rest'')
        _ -> (expr, rest)

parseAssignment :: [Token] -> (Expression, [Token])
parseAssignment tokens = case tokens of
  (Token IDENTIFIER name _ _ : Token EQUAL _ _ _ : rest') ->
    let (expr, rest'') = parseLogicalOperators rest'
     in (Assignment name expr, rest'')
  _ -> parseLogicalOperators tokens

parseStatement :: [Token] -> (Statement, [Token])
parseStatement tokens =
  case tokens of
    (Token PRINT _ _ _ : rest) ->
      let (expr, rest') = parseComparison rest
       in (PrintStatement expr, consumeSemicolon rest')
    (Token WHILE _ _ _ : rest) ->
      let (comp, rest') = parseComparison rest
          (block, rest'') = parseBlock rest'
       in case comp of
            Grouping ifExpr -> (WhileStatement comp block, rest'')
            _ -> error "An if block must be followed by ()"
    (Token IF _ _ _ : rest) ->
      let (comp, rest') = parseComparison rest
          (block, rest'') = parseBlock rest'
       in case comp of
            Grouping ifExpr -> case rest'' of
              (Token ELSE _ _ _ : rest''') ->
                let (block2, rest'''') = parseBlock rest'''
                 in (IfElseStatement ifExpr block block2, rest'''')
              _ -> (IfStatement ifExpr block, rest'')
            _ -> error "An if block must be followed by ()"
    _ ->
      let (expr, rest) = parseAssignment tokens
       in (ExpressionStatement expr, consumeSemicolon rest)

-- in (ExpressionStatement expr, consumeSemicolon rest)

parseDeclaration :: [Token] -> (Declaration, [Token])
parseDeclaration tokens =
  case tokens of
    (Token VAR _ _ _ : rest) -> case rest of
      (Token IDENTIFIER name _ _ : Token EQUAL _ _ _ : rest') ->
        let (stmt, rest'') = parseComparison rest'
         in (VarDeclaration name stmt, consumeSemicolon rest'')
    c@(Token LEFT_BRACE _ _ _ : rest) ->
      let (decs, rest') = parseBlock c
       in (StatementDeclaration (Block decs), rest')
    _ ->
      let (stmt, rest) = parseStatement tokens
       in (StatementDeclaration stmt, rest)

parseBlock :: [Token] -> ([Declaration], [Token])
parseBlock [] = ([], [])
parseBlock (Token LEFT_BRACE _ _ _ : tokens) =
  let parseAll decls tokens =
        case tokens of
          (Token RIGHT_BRACE _ _ _ : rest) -> (reverse decls, rest)
          _ ->
            let (decl, rest') = parseDeclaration tokens
             in parseAll (decl : decls) rest'
   in parseAll [] tokens

consumeSemicolon :: [Token] -> [Token]
consumeSemicolon (Token SEMICOLON _ _ _ : rest) = rest
consumeSemicolon (_ : rest) = error ("Expected semi colon" ++ show rest)

parseProgram :: [Token] -> Program
parseProgram tokens =
  let parseAll decs tokens =
        let (dec, rest) = parseDeclaration tokens
         in case rest of
              (Token EOF _ _ _ : _) -> Program (decs ++ [dec])
              _ -> parseAll (decs ++ [dec]) rest
   in parseAll [] tokens

parse :: [Token] -> Program
parse = parseProgram
