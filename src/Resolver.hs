{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Resolver () where

import Control.Monad.Extra (when)
import Control.Monad.State (State, gets, modify)
import Data.List (uncons)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Parser (Expression (..), Stmt (..))
import Token (Token (..))

type Scope = M.Map Text Bool

data ResolverError = ResolverError
    { token :: Token
    , errorMessage :: Text
    }
    deriving (Show)

data ResolverCtx = ResolverCtx
    { scopes :: [Scope]
    , errors :: [ResolverError]
    }
    deriving (Show)

type Resolver a = State ResolverCtx a

newResolverError :: Token -> Text -> ResolverError
newResolverError = ResolverError

modifyErrors :: [ResolverError] -> Resolver ()
modifyErrors errs = modify (\x -> x{errors = errs})

reportError :: Token -> Text -> Resolver ()
reportError tkn msg = gets errors >>= \errs -> modifyErrors (newResolverError tkn msg : errs)

newScope :: Scope
newScope = M.empty

scopeInsert :: Text -> Bool -> Scope -> Scope
scopeInsert = M.insert

scopeLookup :: Text -> Scope -> Bool
scopeLookup k scp = fromMaybe False (M.lookup k scp)

modifyScope :: [Scope] -> Resolver ()
modifyScope s = modify (\x -> x{scopes = s})

beginScope :: Resolver ()
beginScope = gets scopes >>= (\x -> modifyScope (newScope : x))

endScope :: Resolver ()
endScope = gets scopes >>= process
  where
    process [] = pure ()
    process (_ : ss) = modifyScope ss

scopesPeek :: Resolver (Maybe Scope)
scopesPeek = gets scopes >>= \x -> pure $ fst <$> uncons x

scopePeekLookup :: Text -> Resolver (Maybe Bool)
scopePeekLookup k =
    scopesPeek >>= \case
        Nothing -> pure Nothing
        Just scp -> pure $ Just (scopeLookup k scp)

scopePut :: Text -> Bool -> Resolver ()
scopePut k v =
    gets scopes >>= \case
        [] -> pure ()
        (e : es) -> modifyScope (scopeInsert k v e : es)

visitStmt :: Stmt -> Resolver ()
visitStmt (Block stmts) = visitBlock stmts
visitStmt (Var tkn expr) = visitVarStmt tkn expr
visitStmt (FunctionStmt name params body) = visitFunctionStmt name params body
visitStmt (IfStmt expr ifBranch mElseBranch) = visitIfStmt expr ifBranch mElseBranch
visitStmt (WhileStmt expr stmt) = visitExpr expr >> visitStmt stmt
visitStmt (Return _ mexpr) = maybe (pure ()) visitExpr mexpr
visitStmt (Expression expr) = visitExpr expr
visitStmt (Print expr) = visitExpr expr

visitStmts :: [Stmt] -> Resolver ()
visitStmts = mapM_ visitStmt

visitBlock :: [Stmt] -> Resolver ()
visitBlock stmts = beginScope >> visitStmts stmts >> endScope

visitVarStmt :: Token -> Expression -> Resolver ()
visitVarStmt tkn expr = declare (lexeme tkn) >> visitExpr expr >> define (lexeme tkn)

visitFunctionStmt :: Token -> [Token] -> [Stmt] -> Resolver ()
visitFunctionStmt name params body =
    declare (lexeme name)
        >> define (lexeme name)
        >> resolveFunction name params body

resolveFunction :: Token -> [Token] -> [Stmt] -> Resolver ()
resolveFunction name params body =
    beginScope
        >> mapM_ resolveFunctionParam params
        >> visitStmts body
        >> endScope

visitIfStmt :: Expression -> Stmt -> Maybe Stmt -> Resolver ()
visitIfStmt expr thenBr Nothing = visitExpr expr >> visitStmt thenBr
visitIfStmt expr thenBr (Just elseBr) = visitExpr expr >> visitStmt thenBr >> visitStmt elseBr

resolveFunctionParam :: Token -> Resolver ()
resolveFunctionParam tkn = declare (lexeme tkn) >> define (lexeme tkn)

visitExpr :: Expression -> Resolver ()
visitExpr (Variable tkn) = visitVariableExpr tkn
visitExpr (Assign tkn expr) = visitAssignExpr tkn expr
visitExpr _ = undefined

visitVariableExpr :: Token -> Resolver ()
visitVariableExpr tkn = do
    peekLookupResult <- scopePeekLookup (lexeme tkn)

    when
        (peekLookupResult == Just False)
        (reportError tkn "Can't read local variable in its own initializer.")

    resolveLocal (Variable tkn) tkn

visitAssignExpr :: Token -> Expression -> Resolver ()
visitAssignExpr tkn expr = visitExpr expr >> resolveLocal (Assign tkn expr) tkn

resolveLocal :: Expression -> Token -> Resolver ()
resolveLocal = undefined

declare :: Text -> Resolver ()
declare k = scopePut k False

define :: Text -> Resolver ()
define k = scopePut k True
