module Scanner (
  TokenType (..)
) where

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

data Value = StringValue String | IntValue Int
    deriving (Eq, Show)

data Token = Token
    { tokenType     :: TokenType
    , lexeme        :: String
    , literal       :: Value

    -- Location information
    , line          :: Int
    , offset        :: Int
    , length'       :: Int
    } deriving (Show)
