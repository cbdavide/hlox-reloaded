{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser (
    Expression (..),
    ParseError (..),
    Stmt (Expression, Print, Var, Block, IfStmt, WhileStmt, FunctionStmt),
    parse,
    parseExpression,
    parseStmt,
    parseStmts,
) where

import Control.Monad (unless, void)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.Extra (ifM, whenM, (||^))
import Control.Monad.State (State, evalState, execState, gets, modify, runState)
import Data.Either (isLeft)
import Data.List (uncons)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Runtime (Value (..))
import Token (LiteralValue (LiteralNumber, LiteralString), Token (..), TokenType (..))

data Expression
    = Literal Value
    | Unary Token Expression
    | Logical Expression Token Expression
    | Binary Expression Token Expression
    | Call Expression Token [Expression]
    | Grouping Expression
    | Variable Token
    | Assign Token Expression
    deriving (Eq, Show)

{-# COMPLETE Expression, Print, Var, Block, IfStmt, WhileStmt, FunctionStmt #-}
data Stmt
    = Expression Expression
    | Print Expression
    | Return Token (Maybe Expression)
    | Var Token Expression
    | Block [Stmt]
    | IfStmt Expression Stmt (Maybe Stmt)
    | WhileStmt Expression Stmt
    | FunctionStmt Token [Token] [Stmt]
    | -- Used to recover from errors
      NOP
    deriving (Eq, Show)

data ParserContext = ParserContext
    { source :: [Token]
    , errors :: [ParseError]
    , outputs :: [Stmt]
    }
    deriving (Eq, Show)

data ParseError = ParseError
    { errorMessage :: T.Text
    , token :: Maybe Token
    }
    deriving (Eq, Show)

type Parser a = ExceptT ParseError (State ParserContext) a

modifySource :: [Token] -> Parser ()
modifySource ts = modify (\x -> x{source = ts})

modifyErrors :: [ParseError] -> Parser ()
modifyErrors es = modify (\x -> x{errors = es})

modifyOutputs :: [Stmt] -> Parser ()
modifyOutputs os = modify (\x -> x{outputs = os})

appendError :: ParseError -> Parser ()
appendError e = gets errors >>= (\x -> modifyErrors (x ++ [e]))

appendOutput :: Stmt -> Parser ()
appendOutput o = gets outputs >>= (\x -> modifyOutputs (x ++ [o]))

reportError :: T.Text -> Parser a
reportError msg = peek >>= \mt -> throwError $ ParseError{errorMessage = msg, token = mt}

reportErrorWithToken :: T.Text -> Token -> Parser a
reportErrorWithToken msg tok = throwError $ ParseError{errorMessage = msg, token = Just tok}

addError :: T.Text -> Parser ()
addError msg = peek >>= \mt -> appendError $ ParseError{errorMessage = msg, token = mt}

addErrorWithToken :: T.Text -> Token -> Parser ()
addErrorWithToken msg tkn = appendError $ ParseError{errorMessage = msg, token = Just tkn}

advance :: Parser (Maybe Token)
advance = do
    tok <- gets (uncons . source)

    case tok of
        Nothing -> return Nothing
        Just (x, xs) -> modifySource xs >> return (Just x)

{- | Advances a position in the source.
  This function throws an error when there are no more tokens to consume.
  Use it only when you're certain there are tokens left — for example,
  immediately after a call to 'match' that returned 'True'.
-}
unsafeAdvance :: Parser Token
unsafeAdvance = do
    tok <- advance

    case tok of
        Just t -> return t
        Nothing -> reportError "Unexpected end of tokens"

peek :: Parser (Maybe Token)
peek = gets (fmap fst . uncons . source)

match :: [TokenType] -> Parser Bool
match ts = maybe False ((`elem` ts) . tokenType) <$> peek

isAtEnd :: Parser Bool
isAtEnd = gets source >>= func
  where
    func [Token{tokenType = EOF}] = pure True
    func _ = pure False

declaration :: Parser Stmt
declaration =
    peek >>= \case
        Nothing -> reportError "Unexpected end of tokens"
        Just tkn -> case tokenType tkn of
            VAR -> varDeclaration
            FUN -> advance >> functionDeclaration "function"
            _ -> statement

functionDeclaration :: String -> Parser Stmt
functionDeclaration kind = do
    identifier <- consume IDENTIFIER $ T.pack ("Expected " ++ kind ++ " name")
    _ <- consume LEFT_PAREN $ T.pack ("Expected '(' after " ++ kind ++ " name")

    parameters <- functionParameters
    closeParen <- consume RIGHT_PAREN $ T.pack ("Expected ')' after " ++ kind ++ " name")

    unless (length parameters < 255) (addErrorWithToken "Can't have more than 255 parameters" closeParen)

    _ <- consume LEFT_BRACE $ T.pack ("Expected '{' before " ++ kind ++ " body")

    FunctionStmt identifier parameters <$> block

functionParameters :: Parser [Token]
functionParameters = ifM (match [RIGHT_PAREN]) (return []) (reverse <$> go [])
  where
    go xs =
        consume IDENTIFIER "Expected parameter name" >>= \x ->
            ifM (match [COMMA]) (advance >> go (x : xs)) (return $ x : xs)

varDeclaration :: Parser Stmt
varDeclaration = do
    identifier <- unsafeAdvance >> consume IDENTIFIER "Expected variable name"
    expr <- ifM (match [EQUAL]) (unsafeAdvance >> expression) (return $ Literal Nil)
    _ <- consume SEMICOLON "Expected ';' after variable declaration"
    return $ Var identifier expr

statement :: Parser Stmt
statement =
    peek >>= \case
        Nothing -> reportError "Unexpected end of tokens"
        Just tkn -> case tokenType tkn of
            FOR -> advance >> forStmt
            IF -> advance >> ifStmt
            PRINT -> advance >> printStmt
            RETURN -> returnStmt
            WHILE -> advance >> whileStmt
            LEFT_BRACE -> advance >> Block <$> block
            _ -> expressionStmt

printStmt :: Parser Stmt
printStmt = do
    expr <- expression
    _ <- consume SEMICOLON "Expected ';' after value"
    return $ Print expr

returnStmt :: Parser Stmt
returnStmt =
    Return
        <$> unsafeAdvance -- return keyword token
        <*> returnValueExpr
        <* consume SEMICOLON "Expected ';' after value"

returnValueExpr :: Parser (Maybe Expression)
returnValueExpr = ifM (match [SEMICOLON]) (pure Nothing) (Just <$> expression)

expressionStmt :: Parser Stmt
expressionStmt = do
    expr <- expression
    _ <- consume SEMICOLON "Expected ';' after value"
    return $ Expression expr

ifStmt :: Parser Stmt
ifStmt = do
    _ <- consume LEFT_PAREN "Expected '(' after 'if'"
    condition <- expression
    _ <- consume RIGHT_PAREN "Expected ')' after 'if'"

    IfStmt condition
        <$> statement
        <*> ifM (match [ELSE]) (Just <$> (advance >> statement)) (return Nothing)

forStmt :: Parser Stmt
forStmt = do
    _ <- consume LEFT_PAREN "Expected '(' after 'for'"

    ini <- forInitializerStmt
    cond <- forConditionExpression
    incr <- forIncrementExpression
    body <- statement

    applyForIncrement incr body
        >>= applyForCondition cond
        >>= applyForInitializer ini

forInitializerStmt :: Parser (Maybe Stmt)
forInitializerStmt =
    peek >>= \case
        Nothing -> reportError "Unexpected end of tokens"
        Just tkn -> case tokenType tkn of
            SEMICOLON -> advance >> return Nothing
            VAR -> Just <$> varDeclaration
            _ -> Just <$> expressionStmt

forConditionExpression :: Parser (Maybe Expression)
forConditionExpression = do
    cond <-
        ifM
            (not <$> match [SEMICOLON])
            (Just <$> expression)
            (return Nothing)
    _ <- consume SEMICOLON "Expected ';' after loop condition"
    return cond

forIncrementExpression :: Parser (Maybe Expression)
forIncrementExpression = do
    cond <-
        ifM
            (not <$> match [RIGHT_PAREN])
            (Just <$> expression)
            (return Nothing)
    _ <- consume RIGHT_PAREN "Expected ')' after for clauses"
    return cond

applyForIncrement :: Maybe Expression -> Stmt -> Parser Stmt
applyForIncrement Nothing stmt = return stmt
applyForIncrement (Just incr) stmt = return $ Block [stmt, Expression incr]

applyForCondition :: Maybe Expression -> Stmt -> Parser Stmt
applyForCondition Nothing stmt = return $ WhileStmt (Literal (BooleanValue True)) stmt
applyForCondition (Just cond) stmt = return $ WhileStmt cond stmt

applyForInitializer :: Maybe Stmt -> Stmt -> Parser Stmt
applyForInitializer Nothing stmt = return stmt
applyForInitializer (Just ini) stmt = return $ Block [ini, stmt]

whileStmt :: Parser Stmt
whileStmt = do
    _ <- consume LEFT_PAREN "Expected '(' after 'while'"
    condition <- expression
    _ <- consume RIGHT_PAREN "Expected ')' after 'while'"

    WhileStmt condition <$> statement

block :: Parser [Stmt]
block = reverse <$> go []
  where
    go xs =
        ifM
            (match [RIGHT_BRACE] ||^ isAtEnd)
            (consume RIGHT_BRACE "Expected '}' after a block" >> return xs)
            (declaration >>= \x -> go (x : xs))

expression :: Parser Expression
expression = assignment

assignment :: Parser Expression
assignment = do
    expr <- orExpr

    ifM (not <$> match [EQUAL]) (return expr) $ do
        equals <- unsafeAdvance
        value <- assignment

        case expr of
            Variable name -> return $ Assign name value
            _ -> addErrorWithToken "Invalid assignment target" equals >> return expr

orExpr :: Parser Expression
orExpr = manyLogical1 andExpr [OR]

andExpr :: Parser Expression
andExpr = manyLogical1 equality [AND]

equality :: Parser Expression
equality = manyBinary1 comparison [BANG_EQUAL, EQUAL_EQUAL]

comparison :: Parser Expression
comparison = manyBinary1 term [GREATER, GREATER_EQUAL, LESS, LESS_EQUAL]

term :: Parser Expression
term = manyBinary1 factor [MINUS, PLUS]

factor :: Parser Expression
factor = manyBinary1 unary [SLASH, STAR]

unary :: Parser Expression
unary = ifM (not <$> match [BANG, MINUS]) call (Unary <$> unsafeAdvance <*> unary)

call :: Parser Expression
call = primary >>= \expr -> process expr
  where
    process expr =
        ifM
            (match [LEFT_PAREN])
            (advance >> finishCall expr >>= process)
            (return expr)

finishCall :: Expression -> Parser Expression
finishCall expr = do
    args <- getCallArguments
    unless (length args < 255) (addError "Can't have more than 255 arguments")
    paren <- consume RIGHT_PAREN "Expected ')' after arguments"
    return $ Call expr paren args

getCallArguments :: Parser [Expression]
getCallArguments = ifM (match [RIGHT_PAREN]) (return []) (reverse <$> go [])
  where
    go es =
        expression >>= \e ->
            ifM (match [COMMA]) (advance >> go (e : es)) (return $ e : es)

primary :: Parser Expression
primary =
    advance >>= \case
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

literal' :: Value -> Parser Expression
literal' l = return $ Literal l

numberLiteral' :: Token -> Parser Expression
numberLiteral' Token{literal = LiteralNumber y} = return $ Literal (NumberValue y)
numberLiteral' t = reportErrorWithToken "Failed to parse numeric literal" t

stringLiteral' :: Token -> Parser Expression
stringLiteral' Token{literal = LiteralString y} = return $ Literal (StringValue y)
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

    ifM
        (match [SEMICOLON])
        (void advance) -- consume the token, next one is the start of a statement
        (whenM (not <$> (isAtEnd ||^ match statement_start)) (advance >> synchronize))

manyBinary1 :: Parser Expression -> [TokenType] -> Parser Expression
manyBinary1 rule tokenTypes = rule >>= go
  where
    go expr =
        ifM
            (not <$> match tokenTypes)
            (return expr)
            (Binary expr <$> unsafeAdvance <*> rule >>= go)

manyLogical1 :: Parser Expression -> [TokenType] -> Parser Expression
manyLogical1 rule tokenTypes = rule >>= go
  where
    go expr =
        ifM
            (not <$> match tokenTypes)
            (return expr)
            (Logical expr <$> unsafeAdvance <*> rule >>= go)

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
  where
    ctx = ParserContext{source = tokens, outputs = [], errors = []}
    errors' = errors result
    outputs' = outputs result
    result = execState (runExceptT parseStmts) ctx

parseExpression :: [Token] -> Either ParseError Expression
parseExpression tokens = out
  where
    ctx = ParserContext{source = tokens, outputs = [], errors = []}
    (res, state) = runState (runExceptT expression) ctx
    out
        | isLeft res = res
        | Just (e, _) <- uncons $ errors state = Left e
        | otherwise = res

parseStmt :: [Token] -> Either ParseError Stmt
parseStmt tokens = evalState (runExceptT declaration) ctx
  where
    ctx = ParserContext{source = tokens, outputs = [], errors = []}
