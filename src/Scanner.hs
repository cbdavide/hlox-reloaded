{-# LANGUAGE  OverloadedStrings #-}

module Scanner (
  Token (..)
, TokenType (..)
, ScannerContext (..)
, advance
, advanceIfMatches
, scanTokens
) where

import Control.Monad.State (State, gets, modify)
import Control.Monad.Extra (ifM, unless)
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
    } deriving (Eq, Show)


data Error = Error
    { errorOffset       :: Int
    , errorLength       :: Int
    } deriving (Eq, Show)


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
    } deriving (Eq, Show)

type ScannerState a = State ScannerContext a

isAtEnd :: ScannerState Bool
isAtEnd =  gets (T.null .source)

isNotAtEnd :: ScannerState Bool
isNotAtEnd = not <$> isAtEnd

modifySource :: T.Text -> ScannerState ()
modifySource s' = modify (\x -> x { source = s' })

modifyTokens :: [Token] -> ScannerState ()
modifyTokens t' = modify (\x -> x { tokens = t' })

modifyLexeme :: T.Text -> ScannerState ()
modifyLexeme l' = modify (\x -> x { currentLexeme = l' })

-- Takes the next character from the source, if any and updates the context
advance :: ScannerState (Maybe Char)
advance = do
    result <- gets (T.uncons . source)
    currLexeme <- gets currentLexeme

    case result of
        Nothing -> return Nothing
        Just (x, xs) -> do
            modifySource xs
            modifyLexeme $ T.append currLexeme (T.singleton x)
            return (Just x)

-- Takes the next character if it satisfies the given function
advanceIfMatches :: (Char -> Bool) -> ScannerState Bool
advanceIfMatches check = do
    result <- gets (T.uncons . source)

    case result of
        Nothing -> return False
        Just (x, _) -> if check x then advance >> return True else return False

advanceUntil :: (Char -> Bool) -> ScannerState ()
advanceUntil f = do
    next <- advance

    case next of
        Nothing -> return ()
        (Just c) -> unless (f c) (advanceUntil f)

ignoreUntil :: (Char -> Bool) -> ScannerState ()
ignoreUntil f = advanceUntil f >> modifyLexeme ""

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
    '!' -> ifM (advanceIfMatches (== '='))
               (addToken BANG_EQUAL)
               (addToken BANG)
    '=' -> ifM (advanceIfMatches (== '='))
               (addToken EQUAL_EQUAL)
               (addToken EQUAL)
    '<' -> ifM (advanceIfMatches (== '='))
               (addToken LESS_EQUAL)
               (addToken LESS)
    '>' -> ifM (advanceIfMatches (== '='))
               (addToken GREATER_EQUAL)
               (addToken GREATER)
    '/' -> ifM (advanceIfMatches (== '/'))
               (ignoreUntil (== '\n'))
               (addToken SLASH)
    _ -> undefined


scanTokens :: ScannerState ()
scanTokens = do
    next <- advance

    case next of
        Nothing -> addToken EOF
        Just c -> processToken c >> scanTokens
