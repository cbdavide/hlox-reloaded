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
    process (s : ss) = modifyScope ss

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
visitStmt _ = undefined

visitExpr :: Expression -> Resolver ()
visitExpr (Variable tkn) = visitVariableExpr tkn
visitExpr _ = undefined

visitVariableExpr :: Token -> Resolver ()
visitVariableExpr tkn = do
    peekLookupResult <- scopePeekLookup (lexeme tkn)

    when
        (peekLookupResult == Just False)
        (reportError tkn "Can't read local variable in its own initializer.")

visitBlock :: [Stmt] -> Resolver ()
visitBlock stmts = beginScope >> mapM_ visitStmt stmts >> endScope

visitVarStmt :: Token -> Expression -> Resolver ()
visitVarStmt tkn expr = declare (lexeme tkn) >> visitExpr expr >> define (lexeme tkn)

declare :: Text -> Resolver ()
declare k = scopePut k False

define :: Text -> Resolver ()
define k = scopePut k True
