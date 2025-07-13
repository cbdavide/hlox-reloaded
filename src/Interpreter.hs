{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Interpreter (
    interpret,
    interpretExpression,
) where

import Control.Monad (unless, void)
import Control.Monad.Except (catchError, runExceptT, throwError)
import Control.Monad.Extra (ifM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (evalStateT, gets, modify)
import qualified Data.Text as T
import Parser (Expression (..), Stmt (..))
import Runtime (
    Callable (..),
    CallableImpl (..),
    Environment,
    Interpreter,
    InterpreterContext (..),
    RuntimeError (..),
    Value (..),
    callableFromValue,
    envAssign,
    envDefine,
    envLookup,
    globalEnvironment,
    isNumber,
    isString,
    isTruthy,
    popFrame,
    pushFrame,
 )

import Token (Token (..), TokenType (..))

data Function = Function
    { fnName :: Token
    , fnParams :: [Token]
    , fnBody :: [Stmt]
    }
    deriving (Eq)

instance CallableImpl Function where
    arity :: Function -> Int
    arity = length . fnParams

    name :: Function -> String
    name = T.unpack . lexeme . fnName

    call :: Function -> [Value] -> Interpreter Value
    call c args = do
        prev <- gets environment
        modifyEnvironment globalEnvironment

        mapM_ (uncurry environmentDefine) (zip (fnParams c) args)

        catchError (evalBlock $ fnBody c) (\err -> modifyEnvironment prev >> throwError err)

        modifyEnvironment prev
        pure Nil

modifyEnvironment :: Environment -> Interpreter ()
modifyEnvironment env = modify (\x -> x{environment = env})

removeEnvironmentFrame :: Interpreter ()
removeEnvironmentFrame = gets environment >>= modifyEnvironment . popFrame

addEnvironmentFrame :: Interpreter ()
addEnvironmentFrame = gets environment >>= modifyEnvironment . pushFrame

environmentDefine :: Token -> Value -> Interpreter ()
environmentDefine tkn value =
    gets environment >>= \env -> do
        case envDefine (lexeme tkn) value env of
            Just e -> modifyEnvironment e
            Nothing -> reportError tkn "internal error: there is no environment defined"

environmentGet :: Token -> Interpreter Value
environmentGet tkn =
    gets environment >>= \env -> do
        case envLookup (lexeme tkn) env of
            Just v -> return v
            Nothing -> reportError tkn ("undefined variable '" <> lexeme tkn <> "'")

environmentAssign :: Token -> Value -> Interpreter ()
environmentAssign tkn value =
    gets environment >>= \env -> do
        case envAssign (lexeme tkn) value env of
            Just e -> modifyEnvironment e
            Nothing -> reportError tkn ("undefined variable '" <> lexeme tkn <> "'")

reportError :: Token -> T.Text -> Interpreter a
reportError t msg = throwError $ RuntimeError t msg

interpret :: [Stmt] -> IO ()
interpret stmts = do
    result <-
        evalStateT
            (runExceptT (mapM_ evalStmt stmts))
            (InterpreterContext{environment = globalEnvironment})

    case result of
        Right _ -> return ()
        -- TODO: Format this error
        Left err -> print err

interpretExpression :: Expression -> IO (Either RuntimeError Value)
interpretExpression expr =
    evalStateT
        (runExceptT (evalExpression expr))
        (InterpreterContext{environment = globalEnvironment})

evalStmt :: Stmt -> Interpreter ()
evalStmt (Expression expr) = void $ evalExpression expr
evalStmt (Print expr) = evalPrintStmt expr
evalStmt (Var tkn expr) = evalVarStmt tkn expr
evalStmt (Block stmts) = evalBlock stmts
evalStmt (IfStmt cond thenBranch elseBranch) = evalIfStmt cond thenBranch elseBranch
evalStmt (WhileStmt cond body) = evalWhileStmt cond body
evalStmt (FunctionStmt nm params body) = evalFunctionStmt nm params body

evalFunctionStmt :: Token -> [Token] -> [Stmt] -> Interpreter ()
evalFunctionStmt nm params body = do
    let fn = Function nm params body
    environmentDefine nm (FunctionValue $ Callable fn)

evalPrintStmt :: Expression -> Interpreter ()
evalPrintStmt expr = evalExpression expr >>= \val -> void (liftIO (print $ show val))

evalVarStmt :: Token -> Expression -> Interpreter ()
evalVarStmt tkn expr = evalExpression expr >>= \val -> environmentDefine tkn val

evalBlock :: [Stmt] -> Interpreter ()
evalBlock stmts = do
    addEnvironmentFrame
    catchError (mapM_ evalStmt stmts) (\err -> removeEnvironmentFrame >> throwError err)
    removeEnvironmentFrame

evalIfStmt :: Expression -> Stmt -> Maybe Stmt -> Interpreter ()
evalIfStmt cond thenBranch elseBranch = do
    result <- isTruthy <$> evalExpression cond

    if result
        then evalStmt thenBranch
        else maybe (return ()) evalStmt elseBranch

evalWhileStmt :: Expression -> Stmt -> Interpreter ()
evalWhileStmt cond body =
    ifM
        (isTruthy <$> evalExpression cond)
        (evalStmt body >> evalWhileStmt cond body)
        (return ())

evalExpression :: Expression -> Interpreter Value
evalExpression (Literal a) = return a
evalExpression (Grouping expr) = evalExpression expr
evalExpression (Unary op expr) = evalUnaryExpression op expr
evalExpression (Binary v1 op v2) = evalBinaryExpression op v1 v2
evalExpression (Variable tkn) = environmentGet tkn
evalExpression (Assign tkn expr) = evalAssignment tkn expr
evalExpression (Logical v1 op v2) = evalLogicalExpression op v1 v2
evalExpression (Call expr paren args) = evalCallExpression expr paren args

extractCallable :: Value -> Token -> Interpreter Callable
extractCallable val tkn = case callableFromValue val of
    Nothing -> reportError tkn (T.pack $ "value '" <> show val <> "' is not callable")
    Just c -> pure c

evalCallExpression :: Expression -> Token -> [Expression] -> Interpreter Value
evalCallExpression callee openParenToken args =
    evalExpression callee >>= \val -> do
        callable' <- extractCallable val openParenToken

        let arityError = "expected " <> show (arity callable') <> " arguments but got " <> show (length args)
        unless (length args == arity callable') (reportError openParenToken (T.pack arityError))

        args' <- mapM evalExpression args
        call callable' args'

evalLogicalExpression :: Token -> Expression -> Expression -> Interpreter Value
evalLogicalExpression op v1 v2 = evalExpression v1 >>= eval
  where
    eval val1
        | tokenType op == OR && isTruthy val1 = return val1
        | tokenType op == AND && not (isTruthy val1) = return val1
        | otherwise = evalExpression v2

evalUnaryExpression :: Token -> Expression -> Interpreter Value
evalUnaryExpression t expr =
    evalExpression expr >>= \val ->
        case tokenType t of
            MINUS -> NumberValue . negate <$> extractNumericOperand t val
            BANG -> return $ BooleanValue (not (isTruthy val))
            _ -> reportError t "unary expression with unexpected operator"

evalBinaryExpression :: Token -> Expression -> Expression -> Interpreter Value
evalBinaryExpression op x y =
    evalExpression x >>= \a ->
        evalExpression y >>= \b ->
            case tokenType op of
                PLUS -> evalAddition op a b
                MINUS -> NumberValue <$> applyNumericOperator (-) op a b
                SLASH -> NumberValue <$> applyNumericOperator (/) op a b
                STAR -> NumberValue <$> applyNumericOperator (*) op a b
                GREATER -> BooleanValue <$> applyNumericOperator (>) op a b
                GREATER_EQUAL -> BooleanValue <$> applyNumericOperator (>=) op a b
                LESS -> BooleanValue <$> applyNumericOperator (<) op a b
                LESS_EQUAL -> BooleanValue <$> applyNumericOperator (<=) op a b
                -- Equality operators support operands of different type
                BANG_EQUAL -> return $ BooleanValue (a /= b)
                EQUAL_EQUAL -> return $ BooleanValue (a == b)
                _ -> reportError op "binary expression with unexpected operator"

evalAssignment :: Token -> Expression -> Interpreter Value
evalAssignment tkn expr = do
    val <- evalExpression expr
    environmentAssign tkn val
    return val

evalAddition :: Token -> Value -> Value -> Interpreter Value
evalAddition op x y
    | isNumber x && isNumber y = NumberValue <$> applyNumericOperator (+) op x y
    | isString x && isString y = StringValue <$> concatStringLiterals op x y
    | otherwise = reportError op "operands must be of the same type"

concatStringLiterals :: Token -> Value -> Value -> Interpreter T.Text
concatStringLiterals _ (StringValue a) (StringValue b) = return $ T.concat [a, b]
concatStringLiterals t _ _ = reportError t "unexpected non string literals"

applyNumericOperator :: (Float -> Float -> a) -> Token -> Value -> Value -> Interpreter a
applyNumericOperator op t x y = extractNumericOperands t x y >>= \(a, b) -> return $ a `op` b

extractNumericOperand :: Token -> Value -> Interpreter Float
extractNumericOperand _ (NumberValue n) = return n
extractNumericOperand t _ = reportError t "operand must be a number"

extractNumericOperands :: Token -> Value -> Value -> Interpreter (Float, Float)
extractNumericOperands _ (NumberValue a) (NumberValue b) = return (a, b)
extractNumericOperands t _ _ = reportError t "operands must be numbers"
