{-# LANGUAGE OverloadedStrings #-}

module Scanner (
    Error (..),
    ScannerContext (..),
    ScannerResult,
    scanTokens,
) where

import Control.Monad.Extra (ifM, when)
import Control.Monad.State (State, execState, gets, modify)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import qualified Data.Text as T
import Token (LiteralValue (..), Token (..), TokenType (..))

data Error = Error
    { errorMessage :: T.Text
    , errorLexeme :: T.Text
    , errorLine :: Int
    , errorColumn :: Int
    , errorLexemeLength :: Int
    }
    deriving (Eq, Show)

data ScannerContext = ScannerContext
    { -- inputs
      source :: T.Text
    , -- state
      line :: Int
    , column :: Int
    , currentLexeme :: T.Text
    , -- outputs
      tokens :: [Token]
    , errors :: [Error]
    }
    deriving (Eq, Show)

type ScannerResult = Either [Error] [Token]

type ScannerState a = State ScannerContext a

kewords :: Map.Map T.Text TokenType
kewords =
    Map.fromList
        [ ("and", AND)
        , ("class", CLASS)
        , ("else", ELSE)
        , ("false", FALSE)
        , ("for", FOR)
        , ("fun", FUN)
        , ("if", IF)
        , ("nil", NIL)
        , ("or", OR)
        , ("print", PRINT)
        , ("return", RETURN)
        , ("super", SUPER)
        , ("this", THIS)
        , ("true", TRUE)
        , ("var", VAR)
        , ("while", WHILE)
        ]

_isAlpha :: Char -> Bool
_isAlpha c
    | isAsciiLower c = True
    | isAsciiUpper c = True
    | c == '_' = True
    | otherwise = False

_isAlphaNumeric :: Char -> Bool
_isAlphaNumeric c = _isAlpha c || isDigit c

modifySource :: T.Text -> ScannerState ()
modifySource s' = modify (\x -> x{source = s'})

modifyTokens :: [Token] -> ScannerState ()
modifyTokens t' = modify (\x -> x{tokens = t'})

modifyErrors :: [Error] -> ScannerState ()
modifyErrors e' = modify (\x -> x{errors = e'})

modifyLexeme :: T.Text -> ScannerState ()
modifyLexeme l' = modify (\x -> x{currentLexeme = l'})

modifyColumn :: Int -> ScannerState ()
modifyColumn n' = modify (\x -> x{column = n'})

modifyLine :: Int -> ScannerState ()
modifyLine n' = modify (\x -> x{line = n'})

incrementLine :: ScannerState ()
incrementLine = gets line >>= (\x -> modifyLine (x + 1))

incrementColumnWith :: (Int -> Int) -> ScannerState ()
incrementColumnWith f = gets column >>= modifyColumn . f

incrementColumn :: ScannerState ()
incrementColumn = incrementColumnWith (+ 1)

resetColumn :: ScannerState ()
resetColumn = modify (\x -> x{column = 1})

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

peekNext :: ScannerState (Maybe Char)
peekNext = gets source >>= \x -> return $ getSecondElement x
  where
    getSecondElement t = T.uncons t >>= \x -> fst <$> T.uncons (snd x)

-- Takes the next character if it satisfies the given function
advanceIfMatches :: (Char -> Bool) -> ScannerState Bool
advanceIfMatches check =
    peek >>= \result -> do
        case result of
            Nothing -> return False
            Just x -> if check x then advance >> return True else return False

advanceUntil :: (Char -> Bool) -> ScannerState ()
advanceUntil f = advanceIfMatches (not . f) >>= \x -> when x (advanceUntil f)

advanceWhile :: (Char -> Bool) -> ScannerState ()
advanceWhile f = advanceUntil (not . f)

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
        <*> calculateLexemeStartColumn
        <*> gets (T.length . currentLexeme)

createToken :: TokenType -> ScannerState Token
createToken tp =
    gets (Token tp . currentLexeme)
        <*> getTokenValue tp
        <*> gets line
        <*> calculateLexemeStartColumn
        <*> gets (T.length . currentLexeme)

calculateLexemeStartColumn :: ScannerState Int
calculateLexemeStartColumn = do
    lexemeLength' <- gets (T.length . currentLexeme)
    col <- gets column
    return $ col - lexemeLength' + 1

getTokenValue :: TokenType -> ScannerState LiteralValue
getTokenValue STRING = gets currentLexeme >>= \x -> return $ LiteralString (getStringValue x)
getTokenValue NUMBER = gets currentLexeme >>= \x -> return $ LiteralNumber (read $ T.unpack x)
getTokenValue _ = return NoValue

getStringValue :: T.Text -> T.Text
getStringValue l = T.takeWhile (/= '"') $ T.drop 1 l

addToken :: TokenType -> ScannerState ()
addToken tp = createToken tp >>= appendToken >> modifyLexeme ""

addError :: T.Text -> ScannerState ()
addError m = createError m >>= appendError >> modifyLexeme ""

scanAndAddStringToken :: ScannerState ()
scanAndAddStringToken = do
    advanceUntil (\x -> x == '"' || x == '\n')
    closesString <- advanceIfMatches (== '"')

    if closesString
        then addToken STRING
        else addError "unterminated string"

scanAndAddNumericToken :: ScannerState ()
scanAndAddNumericToken = do
    advanceWhile isDigit

    isNextADot <- (fmap . fmap) (== '.') peek
    isSndADigit <- (fmap . fmap) isDigit peekNext

    when (isJust isNextADot && isJust isSndADigit) $
        advance >> advanceWhile isDigit

    addToken NUMBER

scanAndAddIdentifier :: ScannerState ()
scanAndAddIdentifier = do
    advanceWhile _isAlphaNumeric

    lexeme' <- gets currentLexeme
    let identifier = Map.lookup lexeme' kewords

    case identifier of
        Nothing -> addToken IDENTIFIER
        (Just x) -> addToken x

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
    '!' ->
        ifM
            (advanceIfMatches (== '='))
            (addToken BANG_EQUAL)
            (addToken BANG)
    '=' ->
        ifM
            (advanceIfMatches (== '='))
            (addToken EQUAL_EQUAL)
            (addToken EQUAL)
    '<' ->
        ifM
            (advanceIfMatches (== '='))
            (addToken LESS_EQUAL)
            (addToken LESS)
    '>' ->
        ifM
            (advanceIfMatches (== '='))
            (addToken GREATER_EQUAL)
            (addToken GREATER)
    '/' ->
        ifM
            (advanceIfMatches (== '/'))
            (ignoreUntil (== '\n'))
            (addToken SLASH)
    ' ' -> ignore
    '\r' -> ignore
    '\t' -> ignore
    '\n' -> ignore
    '"' -> scanAndAddStringToken
    _
        | isDigit c -> scanAndAddNumericToken
        | _isAlpha c -> scanAndAddIdentifier
        | otherwise -> addError "unexpected character"

scanTokens' :: ScannerState ()
scanTokens' = do
    next <- advance

    case next of
        Nothing -> addToken EOF
        Just c -> processToken c >> scanTokens'

scanTokens :: T.Text -> ScannerResult
scanTokens s = if not (null errors') then Left errors' else Right tokens'
  where
    ctx = ScannerContext{source = s, line = 1, column = 0, currentLexeme = "", tokens = [], errors = []}
    resultCtx = execState scanTokens' ctx
    errors' = errors resultCtx
    tokens' = tokens resultCtx
