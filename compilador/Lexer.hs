module Lexer (Token(..)) where

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
