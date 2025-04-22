{-# LANGUAGE  OverloadedStrings, PatternSynonyms #-}

module ParserSpec ( parserSpecs ) where

import Control.Monad ( forM_ )
import Test.Hspec ( describe, it, shouldBe, Spec, Expectation, expectationFailure )

import Scanner ( Token (..), TokenType (..), Value (..) )
import Parser ( Expression (..), ParseError (..), LiteralValue(..), parse )
import qualified Data.Text as T

baseToken :: Token
baseToken = Token 
    { tokenType=EOF
    , tokenLine=0
    , tokenLength=0
    , tokenColumn=0
    , lexeme=""
    , literal=EmptyValue
    }

pattern BoolExpr :: Bool -> Expression
pattern BoolExpr b = Literal (BooleanValue b)

pattern NumExpr :: Float -> Expression
pattern NumExpr n = Literal (Parser.NumberValue n)

pattern StrExpr :: T.Text -> Expression
pattern StrExpr t = Literal (Parser.StringValue t)

createToken :: TokenType -> Token
createToken tp = baseToken { tokenType = tp }

createTokens :: [TokenType] -> [Token]
createTokens tps = createToken <$> tps

createNumericToken :: Float -> Token
createNumericToken num = (createToken NUMBER) { literal = Scanner.NumberValue num }

createStringToken :: T.Text -> Token
createStringToken s = (createToken STRING) { literal = Scanner.StringValue s }

shouldParseTo :: (Eq a, Show a, Show e) => Either e a -> a -> Expectation
shouldParseTo (Right actual) expected = actual `shouldBe` expected
shouldParseTo (Left err) _ = expectationFailure $ "Expected successful parse but got error: " ++ show err

parserSpecs :: Spec
parserSpecs = describe "Parser" $ do
    spec_parse

spec_parse :: Spec
spec_parse = describe "parse" $ do

    describe "primary expressions" $ do
        
        let cases :: [(Token, Expression)]
            cases = [ (createToken TRUE, BoolExpr True)
                    , (createToken FALSE, BoolExpr False)
                    , (createToken NIL, Literal Nil)
                    , (createNumericToken 25, NumExpr 25)
                    , (createStringToken "hello", StrExpr "hello")
                    ]

        forM_ cases $ \(tp', expr') -> do
            it ("success - parses '" ++ show (tokenType tp') ++ "' primary rule") $ do
                parse [tp'] `shouldParseTo` expr'

        it "success - parses grouping" $ do
            let tokens = createTokens [LEFT_PAREN, TRUE, RIGHT_PAREN]
                expectedExpr = Grouping (Literal $ BooleanValue True)

            parse tokens `shouldParseTo` expectedExpr

    describe "unary expressions" $ do

        it "success - parses '!' unary rule" $ do
            let operator = createToken BANG
                tokens = operator : createTokens [TRUE]
            parse tokens `shouldParseTo` Unary operator (BoolExpr True)

        it "success - parses '-' unary rule" $ do
            let operator = createToken MINUS
                tokens = [operator, createNumericToken 10]
            parse tokens `shouldParseTo` Unary operator (NumExpr 10)

        it "success - parses nested unary rules" $ do
            let operator1 = createToken BANG
                operator2 = createToken MINUS
                tokens = [operator1, operator2, createToken TRUE]
            parse tokens `shouldParseTo` Unary operator1 (Unary operator2 (BoolExpr True))
