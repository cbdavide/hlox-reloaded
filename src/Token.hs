module Token (
  LiteralValue (..)
, Token (..)
, TokenType (..)
) where

import qualified Data.Text as T

data TokenType =
    -- Single-character tokens
    LEFT_PAREN | RIGHT_PAREN | LEFT_BRACE | RIGHT_BRACE |
    COMMA | DOT | MINUS | PLUS | SEMICOLON | SLASH | STAR |

    -- One or two character tokens
    BANG | BANG_EQUAL |
    EQUAL | EQUAL_EQUAL |
    GREATER | GREATER_EQUAL |
    LESS | LESS_EQUAL |

    -- Literals
    IDENTIFIER | STRING | NUMBER |

    -- Keywords
    AND | CLASS | ELSE | FALSE | FUN | FOR | IF | NIL | OR |
    PRINT | RETURN | SUPER | THIS | TRUE | VAR | WHILE |

    EOF

    deriving (Eq, Show)

data LiteralValue =
      LiteralNumber Float
    | LiteralString T.Text
    | NoValue
    deriving (Eq, Show)

data Token = Token
    { tokenType     :: TokenType
    , lexeme        :: T.Text
    , literal       :: LiteralValue

    -- Location info
    , tokenLine     :: Int
    , tokenColumn   :: Int
    , tokenLength   :: Int
    } deriving (Eq, Show)
