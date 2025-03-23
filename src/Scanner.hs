{-# LANGUAGE  OverloadedStrings #-}

module Scanner (
    scanTokens
) where

import Control.Monad.State (State, gets, modify)
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

data Value = StringValue String | IntValue Int
    deriving (Eq, Show)


data Token = Token
    { tokenType     :: TokenType
    , lexeme        :: T.Text
    , literal       :: Value

    -- Location info
    , tokenOffset        :: Int
    , tokenLegnth        :: Int
    } deriving (Show)


data Error = Error
    { errorOffset       :: Int
    , errorLength       :: Int
    } deriving (Show)


data ScannerContext = ScannerContext
    {
    -- inputs
      source        :: T.Text

    -- state
    , line          :: Int
    , currentLexeme :: T.Text

    -- outputs
    , tokens        :: [Token]
    , errors        :: [Error]
    } deriving (Show)

type ScannerState a = State ScannerContext a

isAtEnd :: ScannerState Bool
isAtEnd =  gets (T.null .source)

isNotAtEnd :: ScannerState Bool
isNotAtEnd = not <$> isAtEnd

modifySource :: T.Text -> ScannerState ()
modifySource s' = modify (\x -> x { source = s' })

modifyTokens :: [Token] -> ScannerState ()
modifyTokens t' = modify (\x -> x { tokens = t' })

-- Take the next character from the source if any and update the context
advance :: ScannerState (Maybe Char)
advance = do
    result <- gets (T.uncons . source)

    case result of
        Nothing -> return Nothing
        Just (x, xs) -> modifySource xs >> return (Just x)

-- Take the next character if it satisfies the given function
advanceIfMatches :: (Char -> Bool) -> ScannerState Bool
advanceIfMatches check = do
    result <- gets (T.uncons . source)

    case result of
        Nothing -> return False
        Just (x, _) -> if check x then advance >> return True else return False

appendToken :: Token -> ScannerState ()
appendToken t = gets tokens >>= (\x -> modifyTokens (x ++ [t]))

createToken :: TokenType -> ScannerState Token
createToken tp =
    gets (Token tp . currentLexeme)
    <*> return (StringValue "tmp")
    <*> return 10
    <*> gets (T.length . currentLexeme)

addToken :: TokenType -> ScannerState ()
addToken tp = createToken tp >>= appendToken

processToken :: Char -> ScannerState ()
processToken c = case c of
    '(' -> addToken LEFT_PAREN
    ')' -> addToken RIGHT_PAREN
    '}' -> addToken LEFT_BRACE
    '{' -> addToken RIGHT_BRACE
    ',' -> addToken COMMA
    '.' -> addToken DOT
    '-' -> addToken MINUS
    '+' -> addToken PLUS
    ';' -> addToken SEMICOLON
    '*' -> addToken STAR
    _ -> undefined


scanTokens :: ScannerState ()
scanTokens = do
    next <- advance

    case next of
        Nothing -> return ()
        Just c -> processToken c >> scanTokens
