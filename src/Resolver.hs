{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Resolver (resolve) where

import Control.Monad.Extra (when, whenM)
import Control.Monad.State (State, execState, gets, modify)
import Data.Foldable (for_, traverse_)
import Data.List (findIndex, uncons)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Parser (Expression (..), Stmt (..))
import Token (Token (..))

type Scope = Map Text Bool
type Locals = Map Token Int

data FunctionType = None | Function
    deriving (Eq, Show)

data ResolverError = ResolverError
    { token :: Token
    , errorMessage :: Text
    }
    deriving (Show)

data ResolverContext = ResolverContext
    { scopes :: [Scope]
    , errors :: [ResolverError]
    , locals :: Locals
    , currentFunction :: FunctionType
    }
    deriving (Show)

type Resolver a = State ResolverContext a

resolverContext :: ResolverContext
resolverContext =
    ResolverContext
        { scopes = []
        , errors = []
        , locals = M.empty
        , currentFunction = None
        }

resolve :: [Stmt] -> Either [ResolverError] Locals
resolve stmts = if null errors' then Right locals' else Left errors'
  where
    ctx = resolverContext
    result = execState (visitStmts stmts) ctx
    locals' = locals result
    errors' = reverse $ errors result

newResolverError :: Token -> Text -> ResolverError
newResolverError = ResolverError

modifyErrors :: [ResolverError] -> Resolver ()
modifyErrors errs = modify (\x -> x{errors = errs})

modifyLocals :: Locals -> Resolver ()
modifyLocals l = modify (\x -> x{locals = l})

modifyFunctionType :: FunctionType -> Resolver ()
modifyFunctionType fnType = modify (\x -> x{currentFunction = fnType})

reportError :: Token -> Text -> Resolver ()
reportError tkn msg = gets errors >>= \errs -> modifyErrors (newResolverError tkn msg : errs)

setLocal :: Token -> Int -> Resolver ()
setLocal expr val = gets locals >>= \ls -> modifyLocals (M.insert expr val ls)

newScope :: Scope
newScope = M.empty

scopeMember :: Text -> Scope -> Bool
scopeMember = M.member

scopeInsert :: Text -> Bool -> Scope -> Scope
scopeInsert = M.insert

scopeLookup :: Text -> Scope -> Maybe Bool
scopeLookup = M.lookup

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
        Just scp -> pure $ scopeLookup k scp

scopePut :: Text -> Bool -> Resolver ()
scopePut k v =
    gets scopes >>= \case
        [] -> pure ()
        (e : es) -> modifyScope (scopeInsert k v e : es)

visitStmt :: Stmt -> Resolver ()
visitStmt (Block stmts) = visitBlock stmts
visitStmt (Var tkn expr) = visitVarStmt tkn expr
visitStmt (FunctionStmt name params body) = visitFunctionStmt name params body
visitStmt (ClassStmt name _) = visitClassStmt name
visitStmt (IfStmt expr ifBranch mElseBranch) = visitIfStmt expr ifBranch mElseBranch
visitStmt (WhileStmt expr stmt) = visitExpr expr >> visitStmt stmt
visitStmt (Return tkn mexpr) = visitReturnStmt tkn mexpr
visitStmt (Expression expr) = visitExpr expr
visitStmt (Print expr) = visitExpr expr

visitStmts :: [Stmt] -> Resolver ()
visitStmts = mapM_ visitStmt

visitReturnStmt :: Token -> Maybe Expression -> Resolver ()
visitReturnStmt tkn expr = do
    whenM (gets ((== None) . currentFunction)) (reportError tkn "Can't return from top-level code.")
    traverse_ visitExpr expr

visitBlock :: [Stmt] -> Resolver ()
visitBlock stmts = beginScope >> visitStmts stmts >> endScope

visitVarStmt :: Token -> Expression -> Resolver ()
visitVarStmt tkn expr = declare tkn >> visitExpr expr >> define (lexeme tkn)

visitFunctionStmt :: Token -> [Token] -> [Stmt] -> Resolver ()
visitFunctionStmt name params body =
    declare name
        >> define (lexeme name)
        >> resolveFunction Function params body

visitClassStmt :: Token -> Resolver ()
visitClassStmt name = declare name >> define (lexeme name)

resolveFunction :: FunctionType -> [Token] -> [Stmt] -> Resolver ()
resolveFunction fnType params body = do
    enclosingFunctionType <- gets currentFunction
    modifyFunctionType fnType

    beginScope
    mapM_ resolveFunctionParam params
    visitStmts body
    endScope

    modifyFunctionType enclosingFunctionType

visitIfStmt :: Expression -> Stmt -> Maybe Stmt -> Resolver ()
visitIfStmt expr thenBr Nothing = visitExpr expr >> visitStmt thenBr
visitIfStmt expr thenBr (Just elseBr) = visitExpr expr >> visitStmt thenBr >> visitStmt elseBr

resolveFunctionParam :: Token -> Resolver ()
resolveFunctionParam tkn = declare tkn >> define (lexeme tkn)

visitExpr :: Expression -> Resolver ()
visitExpr (Variable tkn) = visitVariableExpr tkn
visitExpr (Assign tkn expr) = visitAssignExpr tkn expr
visitExpr (Unary _ expr) = visitExpr expr
visitExpr (Binary left _ right) = visitExpr left >> visitExpr right
visitExpr (Logical left _ right) = visitExpr left >> visitExpr right
visitExpr (Call callee _ exprs) = visitExpr callee >> visitExprs exprs
visitExpr (Get expr _) = visitExpr expr
visitExpr (Set expr _ value) = visitExpr expr >> visitExpr value
visitExpr (Grouping expr) = visitExpr expr
visitExpr (Literal _) = pure ()

visitExprs :: [Expression] -> Resolver ()
visitExprs = mapM_ visitExpr

visitVariableExpr :: Token -> Resolver ()
visitVariableExpr tkn = do
    peekLookupResult <- scopePeekLookup (lexeme tkn)

    when
        (peekLookupResult == Just False)
        (reportError tkn "Can't read local variable in its own initializer.")

    resolveLocal tkn

visitAssignExpr :: Token -> Expression -> Resolver ()
visitAssignExpr tkn expr = visitExpr expr >> resolveLocal tkn

resolveLocal :: Token -> Resolver ()
resolveLocal tkn = do
    scps <- gets scopes
    let midx = findIndex (M.member (lexeme tkn)) scps
    -- We pass the number of scopes between the current innermost
    -- scope and the scope where the varaible was found
    for_ midx (setLocal tkn)

declare :: Token -> Resolver ()
declare tkn = do
    scopeList <- gets scopes

    case scopeList of
        [] -> pure ()
        (scope : _) -> do
            let key = lexeme tkn

            when (scopeMember key scope) $
                reportError tkn "Already a variable with this name in this scope."

            scopePut key False

define :: Text -> Resolver ()
define k = scopePut k True
