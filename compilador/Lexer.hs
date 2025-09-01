module Lexer (Token(..), tokenize, printToken) where

import Data.Char (isSpace, isDigit, isAlpha, isAlphaNum)

data Token =
    IntLiteral Int
    | Identifier String
    | Return
    | LeftParen
    | RightParen
    | LeftBrace
    | RightBrace
    | Semicolon
    deriving (Show, Eq)

printToken :: Token -> String
printToken (IntLiteral n) = show n
printToken (Identifier s) = s
printToken Return = "return"
printToken LeftParen = "("
printToken RightParen = ")"
printToken LeftBrace = "{"
printToken RightBrace = "}"
printToken Semicolon = ";"

tokenize :: String -> [Token]
tokenize = go
  where
    go [] = []
    go s = case dropWhile isSpace s of
      [] -> []
      s' -> case matchToken s' of
        Just (tok, rest) -> tok : go rest
        Nothing -> error $ "Unexpected: " ++ take 10 s'

    matchToken s
      | not (null s) && isDigit (head s) =
          let (num, rest) = span isDigit s
          in if null rest || not (isAlphaNum (head rest) || head rest == '_')
             then Just (IntLiteral (read num), rest)
             else Nothing
      | not (null s) && (isAlpha (head s) || head s == '_') =
          let (id, rest) = span (\c -> isAlphaNum c || c == '_') s
          in if null rest || not (isAlphaNum (head rest) || head rest == '_')
             then Just (if id == "return" then Return else Identifier id, rest)
             else Nothing
      | not (null s) && head s == '(' = Just (LeftParen, tail s)
      | not (null s) && head s == ')' = Just (RightParen, tail s)
      | not (null s) && head s == '{' = Just (LeftBrace, tail s)
      | not (null s) && head s == '}' = Just (RightBrace, tail s)
      | not (null s) && head s == ';' = Just (Semicolon, tail s)
      | otherwise = Nothing
