{-# LANGUAGE  OverloadedStrings #-}

module Interpreter (
  RuntimeError (..)
, evalExpression
, interpret
) where

import Control.Monad (void)
import Control.Monad.Except ( ExceptT, throwError, runExceptT )
import qualified Data.Text as T
import Literal ( LiteralValue (..), isNumber, isString, isTruthy  )
import Parser ( Expression (..), Stmt (..) )
import Token ( Token (..), TokenType (..) )
import Control.Monad.IO.Class (MonadIO(liftIO))

data RuntimeError = RuntimeError
    { token         :: Token
    , errorMessage  :: T.Text
    } deriving (Eq, Show)

type EvalResult a = ExceptT RuntimeError IO a

reportError :: Token -> T.Text -> EvalResult a
reportError t msg = throwError $ RuntimeError t msg

interpret :: [Stmt] -> IO ()
interpret stmts = do
    result <- runExceptT (mapM_ evalStmt stmts)

    case result of
        Right _ -> return ()
        Left err -> print err

evalStmt :: Stmt -> EvalResult ()
evalStmt (Expression expr) = void $ evalExpression expr
evalStmt (Print expr) = do
    val <- evalExpression expr
    liftIO $ print (show val)
    return ()

evalExpression :: Expression -> EvalResult LiteralValue
evalExpression (Literal a) = return a
evalExpression (Grouping expr) = evalExpression expr
evalExpression (Unary op expr) = evalUnaryExpression op expr
evalExpression (Binary v1 op v2) = evalBinaryExpression op v1 v2

evalUnaryExpression :: Token -> Expression -> EvalResult LiteralValue
evalUnaryExpression t expr = evalExpression expr >>= \val ->
    case tokenType t of
        MINUS -> NumberValue . negate <$> extractNumericOperand t val
        BANG -> return $ BooleanValue (not (isTruthy val))
        _ -> reportError t "unary expression with unexpected operator"

evalBinaryExpression :: Token -> Expression -> Expression -> EvalResult LiteralValue
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

evalAddition :: Token -> LiteralValue -> LiteralValue -> EvalResult LiteralValue
evalAddition op x y
    | isNumber x && isNumber y = NumberValue <$> applyNumericOperator (+) op x y
    | isString x && isString y = StringValue <$> concatStringLiterals op x y
    | otherwise = reportError op "operands must be of the same type"

concatStringLiterals :: Token -> LiteralValue -> LiteralValue -> EvalResult T.Text
concatStringLiterals _ (StringValue a) (StringValue b) = return $ T.concat [a, b]
concatStringLiterals t _ _ = reportError t "unexpected non string literals"

applyNumericOperator :: (Float -> Float -> a) -> Token -> LiteralValue -> LiteralValue -> EvalResult a
applyNumericOperator op t x y = extractNumericOperands t x y >>= \(a, b) ->  return $ a `op` b

extractNumericOperand :: Token -> LiteralValue -> EvalResult Float
extractNumericOperand _ (NumberValue n) = return n
extractNumericOperand t _ = reportError t "operand must be a number"

extractNumericOperands :: Token -> LiteralValue -> LiteralValue -> EvalResult (Float, Float)
extractNumericOperands _ (NumberValue a) (NumberValue b) = return (a, b)
extractNumericOperands t _ _ = reportError t "operands must be numbers"
