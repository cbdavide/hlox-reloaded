{-# LANGUAGE OverloadedStrings #-}

module Utils (
    bang,
    minus,
    plus,
    andToken,
    orToken,
    true,
    false,
    openParen,
    closeParen,
    numExpr,
    strExpr,
    boolExpr,
    nilExpr,
) where

import qualified Data.Text as T
import Parser (Expression (..))
import Runtime (Value (..))
import Token (LiteralValue (..), Token (..), TokenType (..))

baseToken :: Token
baseToken =
    Token
        { tokenType = EOF
        , tokenLine = 0
        , tokenLength = 0
        , tokenColumn = 0
        , lexeme = ""
        , literal = NoValue
        }

createToken :: TokenType -> Token
createToken tp = baseToken{tokenType = tp}

minus :: Token
minus = createToken MINUS

bang :: Token
bang = createToken BANG

plus :: Token
plus = createToken PLUS

andToken :: Token
andToken = createToken AND

orToken :: Token
orToken = createToken OR

true :: Token
true = createToken TRUE

false :: Token
false = createToken FALSE

openParen :: Token
openParen = createToken LEFT_PAREN

closeParen :: Token
closeParen = createToken RIGHT_PAREN

boolExpr :: Bool -> Expression
boolExpr b = Literal (BooleanValue b)

numExpr :: Float -> Expression
numExpr n = Literal (NumberValue n)

strExpr :: T.Text -> Expression
strExpr t = Literal (StringValue t)

nilExpr :: Expression
nilExpr = Literal Nil
