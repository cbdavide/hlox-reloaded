{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Interpreter (
    interpret,
    interpretExpression,
) where

import Control.Monad (unless, void)
import Control.Monad.Except (catchError, runExceptT, throwError)
import Control.Monad.Extra (ifM, unlessM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (evalStateT, gets, modify)
import qualified Data.Map as M
import qualified Data.Text as T
import Parser (Expression (..), Stmt (..))
import Runtime (
    Callable (..),
    CallableImpl (..),
    Environment,
    Interpreter,
    InterpreterContext (..),
    Locals,
    RuntimeError (..),
    RuntimeInterrupt (..),
    Value (..),
    callableFromValue,
    envDefine,
    frameAssign,
    frameLookup,
    globalEnvironment,
    isNumber,
    isString,
    isTruthy,
    popFrame,
    pushFrame,
 )

import Token (Token (..), TokenType (..))

data LoxFunction = LoxFunction
    { fnName :: Token
    , fnParams :: [Token]
    , fnBody :: [Stmt]
    , fnClosureEnv :: Environment
    }
    deriving (Eq)

instance CallableImpl LoxFunction where
    arity :: LoxFunction -> Int
    arity = length . fnParams

    name :: LoxFunction -> String
    name f = T.unpack $ "<function " <> (lexeme . fnName) f <> ">"

    call :: LoxFunction -> [Value] -> Interpreter Value
    call c args = do
        prev <- gets environment
        closureEnv <- liftIO $ pushFrame (fnClosureEnv c)
        modifyEnvironment closureEnv

        mapM_ (uncurry environmentDefine) (zip (fnParams c) args)

        value <- catchError (evalBlock (fnBody c) >> pure Nil) (handleFunctionInterrupt prev)

        modifyEnvironment prev
        pure value

handleFunctionInterrupt :: Environment -> RuntimeInterrupt -> Interpreter Value
handleFunctionInterrupt _ (ReturnValue _ val) = pure val
handleFunctionInterrupt env err = modifyEnvironment env >> throwError err

modifyEnvironment :: Environment -> Interpreter ()
modifyEnvironment env = modify (\x -> x{environment = env})

removeEnvironmentFrame :: Interpreter ()
removeEnvironmentFrame = gets environment >>= modifyEnvironment . popFrame

addEnvironmentFrame :: Interpreter ()
addEnvironmentFrame = gets environment >>= liftIO . pushFrame >>= modifyEnvironment

environmentDefine :: Token -> Value -> Interpreter ()
environmentDefine tkn value =
    gets environment >>= \env -> do
        unlessM
            (liftIO (envDefine (lexeme tkn) value env))
            (reportError tkn "internal error: there is no environment defined")

environmentGetAt :: Int -> Token -> Interpreter Value
environmentGetAt idx tkn = do
    env <- gets environment

    unless (idx < length env) (reportError tkn "internal error: frame index out of bounds")

    let frame = env !! idx

    result <- liftIO $ frameLookup (lexeme tkn) frame

    case result of
        Just v -> pure v
        Nothing -> reportError tkn "internal error: expected token at specified env frame was not found"

environmentAssignAt :: Int -> Token -> Value -> Interpreter ()
environmentAssignAt idx tkn value = do
    env <- gets environment

    unless (idx < length env) (reportError tkn "internal error: frame index out of bounds")

    let frame = env !! idx

    result <- liftIO $ frameAssign (lexeme tkn) value frame

    if result
        then pure ()
        else reportError tkn "internal error: expected token at specified frame was not found"

reportError :: Token -> T.Text -> Interpreter a
reportError t msg = throwError $ Error (RuntimeError t msg)

throwReturnInterrupt :: Token -> Value -> Interpreter a
throwReturnInterrupt t v = throwError $ ReturnValue t v

interpret :: Locals -> [Stmt] -> IO ()
interpret locals stmts = do
    globalEnv <- globalEnvironment
    result <-
        evalStateT
            (runExceptT (mapM_ evalStmt stmts))
            (InterpreterContext{environment = globalEnv, localsMap = locals})

    case result of
        Right _ -> return ()
        -- TODO: Format this error
        Left err -> print err

interpretExpression :: Expression -> IO (Either RuntimeError Value)
interpretExpression expr = do
    globalEnv <- globalEnvironment
    evalStateT
        (formatEvalError <$> runExceptT (evalExpression expr))
        (InterpreterContext{environment = globalEnv, localsMap = M.empty})

formatEvalError :: Either RuntimeInterrupt Value -> Either RuntimeError Value
formatEvalError (Right val) = pure val
formatEvalError (Left (Error err)) = Left err
formatEvalError (Left (ReturnValue tkn _)) =
    Left $ RuntimeError{token = tkn, errorMessage = "Unexpected Error: Invalid return statement"}

evalStmt :: Stmt -> Interpreter ()
evalStmt (Expression expr) = void $ evalExpression expr
evalStmt (Print expr) = evalPrintStmt expr
evalStmt (Var tkn expr) = evalVarStmt tkn expr
evalStmt (Block stmts) = evalBlock stmts
evalStmt (IfStmt cond thenBranch elseBranch) = evalIfStmt cond thenBranch elseBranch
evalStmt (WhileStmt cond body) = evalWhileStmt cond body
evalStmt (FunctionStmt nm params body) = evalFunctionStmt nm params body
evalStmt (Return t v) = evalReturnStmt t v

evalReturnStmt :: Token -> Maybe Expression -> Interpreter ()
evalReturnStmt t mv = do
    value <- case mv of
        Nothing -> pure Nil
        Just expr -> evalExpression expr

    throwReturnInterrupt t value

evalFunctionStmt :: Token -> [Token] -> [Stmt] -> Interpreter ()
evalFunctionStmt nm params body =
    gets environment >>= \env -> do
        let fn = LoxFunction nm params body env
        environmentDefine nm (FunctionValue $ Callable fn)

evalPrintStmt :: Expression -> Interpreter ()
evalPrintStmt expr = evalExpression expr >>= \val -> void (liftIO (print val))

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
evalExpression (Variable tkn) = evalVariableExpr tkn
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

evalVariableExpr :: Token -> Interpreter Value
evalVariableExpr tkn = do
    envLength <- gets (length . environment)
    locals <- gets localsMap

    let mdistance = M.lookup tkn locals

    case mdistance of
        Nothing -> environmentGetAt (envLength - 1) tkn
        Just d -> environmentGetAt d tkn

evalAssignment :: Token -> Expression -> Interpreter Value
evalAssignment tkn expr = do
    envLength <- gets (length . environment)
    val <- evalExpression expr
    locals <- gets localsMap

    let mdistance = M.lookup tkn locals

    case mdistance of
        Nothing -> environmentAssignAt (envLength - 1) tkn val
        Just d -> environmentAssignAt d tkn val

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
