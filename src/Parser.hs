{-# LANGUAGE  OverloadedStrings, LambdaCase #-}

module Parser (
  Expression (..)
, ParseError (..)
, Stmt (Expression, Print, Var)
, parse
, parseExpression
) where

import Control.Monad (void, unless)
import Control.Monad.Except ( ExceptT, MonadError(..), runExceptT )
import Control.Monad.Extra (ifM, (||^), whenM)
import Control.Monad.State (evalState, State, gets, modify, execState)
import Data.List (uncons)
import qualified Data.Text as T
import Literal ( LiteralValue (..) )
import Token ( Token (..), TokenType (..) )
import Data.Maybe (fromJust)

data Expression = Literal LiteralValue
    | Unary Token Expression
    | Binary Expression Token Expression
    | Grouping Expression
    | Variable Token
    | Assign Token Expression
    deriving (Eq, Show)

{-# COMPLETE Expression, Print, Var #-}
data Stmt = Expression Expression
    | Print Expression
    | Var Token Expression
    -- Used to recover from errors
    | NOP
    deriving (Eq, Show)

data ParserContext = ParserContext
    { source :: [Token]
    , errors :: [ParseError]
    , outputs :: [Stmt]
    } deriving (Eq, Show)

data ParseError = ParseError
    { errorMessage  :: T.Text
    , token         :: Maybe Token
    } deriving (Eq, Show)

type Parser a = ExceptT ParseError (State ParserContext) a

modifySource :: [Token] -> Parser ()
modifySource ts = modify (\x -> x { source = ts })

modifyErrors :: [ParseError] -> Parser ()
modifyErrors es = modify (\x -> x { errors = es })

modifyOutputs :: [Stmt] -> Parser ()
modifyOutputs os = modify (\x -> x { outputs = os })

appendError :: ParseError -> Parser ()
appendError e = gets errors >>= (\x -> modifyErrors (x ++ [e]))

appendOutput :: Stmt -> Parser ()
appendOutput o = gets outputs >>= (\x -> modifyOutputs (x ++ [o]))

reportError :: T.Text -> Parser a
reportError msg = peek >>= \mt -> throwError $ ParseError {errorMessage=msg, token=mt}

reportErrorWithToken :: T.Text -> Token -> Parser a
reportErrorWithToken msg tok =throwError $ ParseError {errorMessage=msg, token=Just tok}

advance :: Parser (Maybe Token)
advance = do
    tok <- gets (uncons . source)

    case tok of
        Nothing -> return Nothing
        Just (x, xs) -> modifySource xs >> return (Just x)

-- | Advances a position in the source.
--   This function throws an error when there are no more tokens to consume.
--   Use it only when you're certain there are tokens left — for example,
--   immediately after a call to 'match' that returned 'True'.
unsafeAdvance :: Parser Token
unsafeAdvance  = do
    tok <- advance
     
    case tok of
        Just t -> return t
        Nothing -> reportError "Unexpected end of tokens"

peek :: Parser (Maybe Token)
peek = gets (fmap fst . uncons . source)

match :: [TokenType] -> Parser Bool
match ts = maybe False ((`elem` ts) . tokenType) <$> peek

isAtEnd :: Parser Bool
isAtEnd = gets (null . source)

declaration :: Parser Stmt
declaration = ifM (match [VAR]) varDeclaration statement

varDeclaration :: Parser Stmt
varDeclaration = do
    identifier <- unsafeAdvance >> consume IDENTIFIER "Expected variable name"
    expr <- ifM (match [EQUAL]) expression (return $ Literal Nil)
    _ <- consume SEMICOLON "Expected ';' after variable declaration"
    return $ Var identifier expr

statement :: Parser Stmt
statement = ifM (match [PRINT]) printStmt expressionStmt

printStmt :: Parser Stmt
printStmt = do
    expr <- unsafeAdvance >> expression
    _ <- consume SEMICOLON "Expected ';' after value"
    return $ Print expr

expressionStmt :: Parser Stmt
expressionStmt = do
    expr <- expression
    _ <- consume SEMICOLON "Expected ';' after value"
    return $ Expression expr

expression :: Parser Expression
expression = assignment

assignment :: Parser Expression
assignment = do
    expr <- equality

    ifM (not <$> match [EQUAL]) (return expr) $ do
        equals <- unsafeAdvance
        value <- assignment

        case expr of
            Variable name -> return $ Assign name value
            _ -> reportErrorWithToken "Invalid assignment target" equals

equality :: Parser Expression
equality = many1 comparison [BANG_EQUAL, EQUAL_EQUAL]

comparison :: Parser Expression
comparison = many1 term [GREATER, GREATER_EQUAL, LESS, LESS_EQUAL]

term :: Parser Expression
term = many1 factor [MINUS, PLUS]

factor :: Parser Expression
factor = many1 unary [SLASH, STAR]

unary :: Parser Expression
unary = ifM (not <$> match [BANG, MINUS]) primary (Unary <$> unsafeAdvance <*> unary)

primary :: Parser Expression
primary = advance >>= \case
        Nothing -> reportError "Expected expression"
        Just t -> case tokenType t of
            FALSE -> literal' (BooleanValue False)
            TRUE -> literal' (BooleanValue True)
            NIL -> literal' Nil
            NUMBER -> numberLiteral' t
            STRING -> stringLiteral' t
            IDENTIFIER -> return $ Variable t
            LEFT_PAREN -> group
            _ -> reportErrorWithToken "Expected expression" t

literal' :: LiteralValue -> Parser Expression
literal' l = return $ Literal l

numberLiteral' :: Token -> Parser Expression
numberLiteral' Token { literal = NumberValue y } = return $ Literal (NumberValue y)
numberLiteral' t = reportErrorWithToken "Failed to parse numeric literal" t

stringLiteral' :: Token -> Parser Expression
stringLiteral' Token { literal = StringValue y } = return $ Literal (StringValue y)
stringLiteral' t = reportErrorWithToken "Failed to parse string literal" t

group :: Parser Expression
group = do
    expr <- expression
    _ <- consume RIGHT_PAREN "Expected ')' after expression"
    return $ Grouping expr

consume :: TokenType -> T.Text -> Parser Token
consume tp msg = ifM (match [tp]) (fromJust <$> advance) (reportError msg)

-- | Discards tokens until it thinks it has found a statement boundary.
synchronize :: Parser ()
synchronize = do
    let statement_start = [CLASS, FUN, VAR, FOR, IF, WHILE, PRINT, RETURN]

    ifM (match [SEMICOLON])
        (void advance) -- consume the token, next one is the start of a statement
        (whenM (not <$> (isAtEnd ||^ match statement_start)) (advance >> synchronize))

many1 :: Parser Expression -> [TokenType] -> Parser Expression
many1 rule tokenTypes = rule >>= go
    where go expr =
            ifM (not <$> match tokenTypes) 
                (return expr) 
                (Binary expr <$> unsafeAdvance <*> rule >>= go)

parseExpression :: [Token] -> Either ParseError Expression
parseExpression tokens = evalState (runExceptT expression) ctx
    where ctx = ParserContext {source=tokens, outputs=[], errors=[]}

parseStmts :: Parser ()
parseStmts =
    ifM isAtEnd (return ()) $ do
        stmt <- catchError declaration handleError
        -- NOP is a flag value that indicates there was an error
        -- parsing a statement, in that case we omit saving it
        -- in the results.
        unless (stmt == NOP) (appendOutput stmt)
        parseStmts

handleError :: ParseError -> Parser Stmt
handleError err = appendError err >> synchronize >> return NOP

parse :: [Token] -> Either [ParseError] [Stmt]
parse tokens = if null errors' then Right outputs' else Left errors'
    where ctx = ParserContext {source=tokens, outputs=[], errors=[]}
          errors' = errors result
          outputs' = outputs result
          result = execState (runExceptT parseStmts) ctx
