{-# LANGUAGE  OverloadedStrings #-}

module Scanner (
  Error
, Token (..)
, TokenType (..)
, ScannerContext (..)
, ScannerResult
, Value (..)
, advance
, advanceIfMatches
, scanTokens
, scanTokens'
) where

import Control.Monad.State (State, gets, modify, execState)
import Control.Monad.Extra (ifM, when)
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
    , tokenLine     :: Int
    , tokenColumn   :: Int
    , tokenLength   :: Int
    } deriving (Eq, Show)


data Error = Error
    { errorMessage  :: T.Text
    , errorLexeme   :: T.Text
    , errorLine     :: Int
    , errorColumn   :: Int
    } deriving (Eq, Show)


data ScannerContext = ScannerContext
    {
    -- inputs
      source        :: T.Text

    -- state
    , line          :: Int
    , column        :: Int
    , currentLexeme :: T.Text

    -- outputs
    , tokens        :: [Token]
    , errors        :: [Error]
    } deriving (Eq, Show)

type ScannerResult = Either [Error] [Token]

type ScannerState a = State ScannerContext a

modifySource :: T.Text -> ScannerState ()
modifySource s' = modify (\x -> x { source = s' })

modifyTokens :: [Token] -> ScannerState ()
modifyTokens t' = modify (\x -> x { tokens = t' })

modifyErrors :: [Error] -> ScannerState ()
modifyErrors e' = modify (\x -> x { errors = e' })

modifyLexeme :: T.Text -> ScannerState ()
modifyLexeme l' = modify (\x -> x { currentLexeme = l' })

modifyColumn :: Int -> ScannerState ()
modifyColumn n' = modify (\x -> x { column = n' })

modifyLine :: Int -> ScannerState ()
modifyLine n' = modify (\x -> x { line = n' })

incrementLine :: ScannerState ()
incrementLine = gets line >>= (\x -> modifyLine (x + 1))

incrementColumnWith :: (Int -> Int) -> ScannerState ()
incrementColumnWith f = gets column >>= modifyColumn . f

incrementColumn :: ScannerState ()
incrementColumn = incrementColumnWith (+ 1)

resetColumn :: ScannerState ()
resetColumn = modify (\x -> x { column = 1 })

modifyPosition :: Char -> ScannerState ()
modifyPosition '\n' = incrementLine >> resetColumn
modifyPosition '\t' = incrementColumnWith (\x -> x + 4 - ((x - 1) `mod` 4))
modifyPosition _ = incrementColumn

-- Takes the next character from the source, if any and updates the context
advance :: ScannerState (Maybe Char)
advance = do
    result <- gets (T.uncons . source)
    currLexeme <- gets currentLexeme

    case result of
        Nothing -> return Nothing
        Just (x, xs) -> do
            modifySource xs
            modifyPosition x
            modifyLexeme $ T.append currLexeme (T.singleton x)
            return (Just x)

peek :: ScannerState (Maybe Char)
peek = gets source >>= \x -> return (fst <$> T.uncons x)

-- Takes the next character if it satisfies the given function
advanceIfMatches :: (Char -> Bool) -> ScannerState Bool
advanceIfMatches check = peek >>= \result -> do
    case result of
        Nothing -> return False
        Just x -> if check x then advance >> return True else return False

advanceUntil :: (Char -> Bool) -> ScannerState ()
advanceUntil f = advanceIfMatches (not . f) >>= \x -> when x (advanceUntil f)

ignoreUntil :: (Char -> Bool) -> ScannerState ()
ignoreUntil f = advanceUntil f >> modifyLexeme ""

ignore :: ScannerState ()
ignore = modifyLexeme ""

appendToken :: Token -> ScannerState ()
appendToken t = gets tokens >>= (\x -> modifyTokens (x ++ [t]))

appendError :: Error -> ScannerState ()
appendError e = gets errors >>= (\x -> modifyErrors (x ++ [e]))

createError :: T.Text -> ScannerState Error
createError m =
        gets (Error m . currentLexeme)
    <*> gets line
    <*> gets column

createToken :: TokenType -> ScannerState Token
createToken tp =
        gets (Token tp . formatLexeme tp . currentLexeme)
    <*> return (StringValue "tmp")
    <*> gets line
    <*> calculateLexemeStartColumn
    <*> gets (T.length . currentLexeme)

calculateLexemeStartColumn :: ScannerState Int
calculateLexemeStartColumn = do
    lexemeLength <- gets (T.length . currentLexeme)
    col <- gets column
    return $ col - lexemeLength + 1

formatLexeme :: TokenType -> T.Text -> T.Text
formatLexeme tp l = case tp of
    STRING -> T.takeWhile (/= '"') $ T.drop 1 l
    _ -> l

addToken :: TokenType -> ScannerState ()
addToken tp = createToken tp >>= appendToken >> modifyLexeme ""

addError :: T.Text -> ScannerState ()
addError m = createError m >>= appendError >> modifyLexeme ""

scanAndAddStringToken :: ScannerState ()
scanAndAddStringToken = do
    advanceUntil (\x -> x == '"' || x == '\n')
    advance -- to get the closing " or \n
    lexeme' <- gets (T.head . T.reverse . currentLexeme)

    if lexeme' == '"'
        then addToken STRING
        else addError "unterminated string"

processToken :: Char -> ScannerState ()
processToken c = case c of
    '(' -> addToken LEFT_PAREN
    ')' -> addToken RIGHT_PAREN
    '{' -> addToken LEFT_BRACE
    '}' -> addToken RIGHT_BRACE
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
    ' '  -> ignore
    '\r' -> ignore
    '\t' -> ignore
    '\n' -> ignore
    '"' -> scanAndAddStringToken
    _ -> undefined


scanTokens' :: ScannerState ()
scanTokens' = do
    next <- advance

    case next of
        Nothing -> addToken EOF
        Just c -> processToken c >> scanTokens'

scanTokens :: T.Text -> ScannerResult
scanTokens s = if not (null errors') then Left errors' else Right tokens'
    where ctx = ScannerContext { source=s, line=1, column=0, currentLexeme="", tokens=[], errors=[] }
          resultCtx = execState scanTokens' ctx
          errors' = errors resultCtx
          tokens' = tokens resultCtx
