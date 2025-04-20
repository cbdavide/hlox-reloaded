{-# LANGUAGE  OverloadedStrings, LambdaCase #-}

module Parser (
  Expression (..)
, LiteralValue (..)
, ParseError (..)
, parse
) where

import Control.Monad (void)
import Control.Monad.Except ( ExceptT, MonadError(..), runExceptT )
import Control.Monad.Extra (ifM, (||^), whenM)
import Control.Monad.State (evalState, State, gets, modify)
import Data.List (uncons)
import qualified Data.Text as T
import Scanner (Token (..), TokenType (..), Value (..))

data LiteralValue =
      NumberValue Float
    | StringValue T.Text  
    | BooleanValue Bool
    | Nil
    deriving (Eq, Show)

data Expression = Literal LiteralValue
    | Unary Token Expression
    | Binary Expression Token Expression
    | Grouping Expression
    deriving (Eq, Show)

data ParserContext = ParserContext
    { source :: [Token]
    , output :: Maybe Expression
    } deriving (Eq, Show)

data ParseError = ParseError
    { errorMessage  :: T.Text
    , token         :: Maybe Token
    } deriving (Eq, Show)

type Parser a = ExceptT ParseError (State ParserContext) a

modifySource :: [Token] -> Parser ()
modifySource ts = modify (\x -> x { source = ts })

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

expression :: Parser Expression
expression = equality

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
            LEFT_PAREN -> group
            _ -> reportError "Expected expression"

literal' :: LiteralValue -> Parser Expression
literal' l = return $ Literal l

numberLiteral' :: Token -> Parser Expression
numberLiteral' Token { literal = Scanner.NumberValue y } = return $ Literal (Parser.NumberValue y)
numberLiteral' t = reportErrorWithToken "Failed to parse numeric literal" t

stringLiteral' :: Token -> Parser Expression
stringLiteral' Token { literal = Scanner.StringValue y } = return $ Literal (Parser.StringValue y)
stringLiteral' t = reportErrorWithToken "Failed to parse string literal" t

group :: Parser Expression
group = do
    expr <- expression
    consume RIGHT_PAREN "Expected ')' after expression"
    return $ Grouping expr

consume :: TokenType -> T.Text -> Parser ()
consume tp msg = ifM (match [tp]) (void advance) (reportError msg)

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

parse :: [Token] -> Either ParseError Expression
parse tokens = evalState (runExceptT expression) ctx
    where ctx = ParserContext {source=tokens, output=Nothing}
