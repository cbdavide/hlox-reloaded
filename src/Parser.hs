{-# LANGUAGE  OverloadedStrings, LambdaCase #-}

module Parser (expression) where

import Control.Monad (void)
import Control.Monad.Except ( ExceptT, MonadError(..) )
import Control.Monad.Extra (ifM)
import Control.Monad.State (State, gets, modify)
import Data.List (uncons)
import qualified Data.Text as T
import Scanner (Token (..), TokenType (..))

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
    , output :: Expression
    } deriving (Eq, Show)

data ParseError = ParseError
    { errorMessage  :: T.Text
    , token         :: Maybe Token
    }

type Parser a = ExceptT ParseError (State ParserContext) a

modifySource :: [Token] -> Parser ()
modifySource ts = modify (\x -> x { source = ts })

reportError :: T.Text -> Parser a
reportError msg = peek >>= \mt -> throwError $ ParseError {errorMessage=msg, token=mt}

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
            NUMBER -> literal' (NumberValue 2)
            STRING -> literal' (StringValue "")
            LEFT_PAREN -> group
            _ -> reportError "Expected expression"

literal' :: LiteralValue -> Parser Expression
literal' l = return $ Literal l

group :: Parser Expression
group = do
    expr <- expression
    consume RIGHT_PAREN "Expected ')' after expression"
    return $ Grouping expr

consume :: TokenType -> T.Text -> Parser ()
consume tp msg = ifM (match [tp]) (void advance) (reportError msg)

many1 :: Parser Expression -> [TokenType] -> Parser Expression
many1 rule tokenTypes = rule >>= go
    where go expr =
            ifM (not <$> match tokenTypes) 
                (return expr) 
                (Binary expr <$> unsafeAdvance <*> rule >>= go)
