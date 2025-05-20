{-# LANGUAGE  OverloadedStrings #-}

module Interpreter (
  RuntimeError (..)
, interpret
, interpretExpression
) where

import Control.Monad (void)
import Control.Monad.Except ( ExceptT, throwError, runExceptT )
import Control.Monad.State ( StateT, evalStateT, modify, gets )
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Literal ( LiteralValue (..), isNumber, isString, isTruthy  )
import Parser ( Expression (..), Stmt (..) )
import Token ( Token (..), TokenType (..) )
import Control.Monad.IO.Class (MonadIO(liftIO))

type Environment = Map Text LiteralValue

newtype InterpreterContext = InterpreterContext
    { environment :: Environment
    } deriving (Eq, Show)

data RuntimeError = RuntimeError
    { token         :: Token
    , errorMessage  :: T.Text
    } deriving (Eq, Show)

type Interpreter a = ExceptT RuntimeError (StateT InterpreterContext IO) a

modifyEnvironment :: Environment -> Interpreter ()
modifyEnvironment env = modify (\x -> x { environment = env })

environmentDefine :: Text -> LiteralValue -> Interpreter ()
environmentDefine name value = do
    env <- gets environment
    modifyEnvironment $ M.insert name value env

reportError :: Token -> T.Text -> Interpreter a
reportError t msg = throwError $ RuntimeError t msg

interpret :: [Stmt] -> IO ()
interpret stmts = do
    result <- evalStateT (runExceptT (mapM_ evalStmt stmts) )
                         (InterpreterContext {environment=M.empty})

    case result of
        Right _ -> return ()
        -- TODO: Format this error
        Left err -> print err

interpretExpression :: Expression -> IO (Either RuntimeError LiteralValue)
interpretExpression expr =
    evalStateT (runExceptT (evalExpression expr))
               (InterpreterContext {environment=M.empty})

evalStmt :: Stmt -> Interpreter ()
evalStmt (Expression expr) = void $ evalExpression expr
evalStmt (Print expr) = evalPrintStmt expr
evalStmt (Var tkn expr) = evalVarStmt tkn expr

evalPrintStmt :: Expression -> Interpreter ()
evalPrintStmt expr = evalExpression expr >>= \val -> void (liftIO (print $ show val))

evalVarStmt :: Token -> Expression -> Interpreter ()
evalVarStmt tkn expr = evalExpression expr >>= \val -> environmentDefine (lexeme tkn) val

evalExpression :: Expression -> Interpreter LiteralValue
evalExpression (Literal a) = return a
evalExpression (Grouping expr) = evalExpression expr
evalExpression (Unary op expr) = evalUnaryExpression op expr
evalExpression (Binary v1 op v2) = evalBinaryExpression op v1 v2

evalUnaryExpression :: Token -> Expression -> Interpreter LiteralValue
evalUnaryExpression t expr = evalExpression expr >>= \val ->
    case tokenType t of
        MINUS -> NumberValue . negate <$> extractNumericOperand t val
        BANG -> return $ BooleanValue (not (isTruthy val))
        _ -> reportError t "unary expression with unexpected operator"

evalBinaryExpression :: Token -> Expression -> Expression -> Interpreter LiteralValue
evalBinaryExpression op x y = evalExpression x >>= \a -> evalExpression y >>= \b ->
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

evalAddition :: Token -> LiteralValue -> LiteralValue -> Interpreter LiteralValue
evalAddition op x y
    | isNumber x && isNumber y = NumberValue <$> applyNumericOperator (+) op x y
    | isString x && isString y = StringValue <$> concatStringLiterals op x y
    | otherwise = reportError op "operands must be of the same type"

concatStringLiterals :: Token -> LiteralValue -> LiteralValue -> Interpreter T.Text
concatStringLiterals _ (StringValue a) (StringValue b) = return $ T.concat [a, b]
concatStringLiterals t _ _ = reportError t "unexpected non string literals"

applyNumericOperator :: (Float -> Float -> a) -> Token -> LiteralValue -> LiteralValue -> Interpreter a
applyNumericOperator op t x y = extractNumericOperands t x y >>= \(a, b) ->  return $ a `op` b

extractNumericOperand :: Token -> LiteralValue -> Interpreter Float
extractNumericOperand _ (NumberValue n) = return n
extractNumericOperand t _ = reportError t "operand must be a number"

extractNumericOperands :: Token -> LiteralValue -> LiteralValue -> Interpreter (Float, Float)
extractNumericOperands _ (NumberValue a) (NumberValue b) = return (a, b)
extractNumericOperands t _ _ = reportError t "operands must be numbers"
