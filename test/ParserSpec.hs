{-# LANGUAGE  OverloadedStrings, PatternSynonyms #-}

module ParserSpec ( parserSpecs ) where

import Control.Monad ( forM_ )
import Literal ( LiteralValue (..) )
import Parser ( Expression (..), ParseError (..), parse )
import Test.Hspec ( describe, it, shouldBe, Spec, Expectation, expectationFailure )
import Token ( Token (..), TokenType (..) )
import qualified Data.Text as T

baseToken :: Token
baseToken = Token 
    { tokenType=EOF
    , tokenLine=0
    , tokenLength=0
    , tokenColumn=0
    , lexeme=""
    , literal=Nil
    }

pattern BoolExpr :: Bool -> Expression
pattern BoolExpr b = Literal (BooleanValue b)

pattern NumExpr :: Float -> Expression
pattern NumExpr n = Literal (NumberValue n)

pattern StrExpr :: T.Text -> Expression
pattern StrExpr t = Literal (StringValue t)

createToken :: TokenType -> Token
createToken tp = baseToken { tokenType = tp }

createTokens :: [TokenType] -> [Token]
createTokens tps = createToken <$> tps

createNumericToken :: Float -> Token
createNumericToken num = (createToken NUMBER) { literal = NumberValue num }

createStringToken :: T.Text -> Token
createStringToken s = (createToken STRING) { literal = StringValue s }

shouldParseTo :: (Eq a, Show a, Show e) => Either e a -> a -> Expectation
shouldParseTo (Right actual) expected = actual `shouldBe` expected
shouldParseTo (Left err) _ = expectationFailure $ "Expected successful parse but got error: " ++ show err

shouldFailTo :: (Eq e, Show e, Show a) => Either e a -> e -> Expectation
shouldFailTo (Right a) _  = expectationFailure $ "Expected error but got successful response :" ++ show a
shouldFailTo (Left err) expected  =  err `shouldBe` expected

parserSpecs :: Spec
parserSpecs = describe "Parser" $ do
    spec_parse

spec_parse :: Spec
spec_parse = describe "parse" $ do

    describe "primary rule" $ do
        
        let cases :: [(Token, Expression)]
            cases = [ (createToken TRUE, BoolExpr True)
                    , (createToken FALSE, BoolExpr False)
                    , (createToken NIL, Literal Nil)
                    , (createNumericToken 25, NumExpr 25)
                    , (createStringToken "hello", StrExpr "hello")
                    ]

        forM_ cases $ \(tp', expr') -> do
            it ("success - parses '" ++ show (tokenType tp') ++ "' literal") $ do
                parse [tp'] `shouldParseTo` expr'

        it "success - parses grouped '(true)' expression" $ do
            let tokens = createTokens [LEFT_PAREN, TRUE, RIGHT_PAREN]
                expectedExpr = Grouping (Literal $ BooleanValue True)

            parse tokens `shouldParseTo` expectedExpr

    describe "unary rule" $ do

        it "success - parses '!true' expression" $ do
            let operator = createToken BANG
                tokens = operator : createTokens [TRUE]
            parse tokens `shouldParseTo` Unary operator (BoolExpr True)

        it "success - parses '-10' expression" $ do
            let operator = createToken MINUS
                tokens = [operator, createNumericToken 10]
            parse tokens `shouldParseTo` Unary operator (NumExpr 10)

        it "success - parses nested '!-true' expression" $ do
            let operator1 = createToken BANG
                operator2 = createToken MINUS
                tokens = [operator1, operator2, createToken TRUE]
            parse tokens `shouldParseTo` Unary operator1 (Unary operator2 (BoolExpr True))

    describe "factor rule" $ do

        it "success - parses '100 * 1' expression" $ do
            let operator = createToken STAR
                tokens = [createNumericToken 100, operator, createNumericToken 25]
            parse tokens `shouldParseTo` Binary (NumExpr 100) operator (NumExpr 25)

        it "success - parses '2 / 0' expression" $ do
            let operator = createToken SLASH
                tokens = [createNumericToken 2, operator, createNumericToken 0]
            parse tokens `shouldParseTo` Binary (NumExpr 2) operator (NumExpr 0)

        it "success - parses '(1 * 2) / 3' expression" $ do
            let operator1 = createToken STAR
                operator2 = createToken SLASH

                tokens = [ createNumericToken 1
                         , operator1
                         , createNumericToken 2
                         , operator2
                         , createNumericToken 3
                         ]

                expectedExpression = Binary
                    (Binary (NumExpr 1) operator1 (NumExpr 2))
                    operator2
                    (NumExpr 3)

            parse tokens `shouldParseTo` expectedExpression

    describe "term rule" $ do

        it "success - parses '22 + 12' expression" $ do
            let operator = createToken PLUS
                tokens = [createNumericToken 22, operator, createNumericToken 12]
            parse tokens `shouldParseTo` Binary (NumExpr 22) operator (NumExpr 12)

        it "success - parses '4 - 3' expression" $ do
            let operator = createToken SLASH
                tokens = [createNumericToken 4, operator, createNumericToken 3]
            parse tokens `shouldParseTo` Binary (NumExpr 4) operator (NumExpr 3)

        it "success - parses '1 + (2 / 3) + 5' expression" $ do
            let sum' = createToken PLUS
                rest' = createToken MINUS
                div' = createToken SLASH

                tokens = [ createNumericToken 1
                         , sum'
                         , createNumericToken 2
                         , div'
                         , createNumericToken 3
                         , rest'
                         , createNumericToken 5
                         ]

                expectedExpression = Binary
                    (Binary (NumExpr 1) sum' (Binary (NumExpr 2) div' (NumExpr 3)))
                    rest'
                    (NumExpr 5)

            parse tokens `shouldParseTo` expectedExpression

    describe "comparison rule" $ do
        let val1 = createNumericToken 10
            val2 = createNumericToken 15
            cases :: [(String, Token)]
            cases = [ (">", createToken GREATER)
                    , (">=", createToken GREATER_EQUAL)
                    , ("<", createToken LESS)
                    , ("<=", createToken LESS_EQUAL)
                    ]

        forM_ cases $ \(s', tp') -> do
            it ("success - parses '10 " ++ s' ++ " 15' expression") $ do
                parse [val1, tp', val2] `shouldParseTo` Binary (NumExpr 10) tp' (NumExpr 15)

    describe "equality rule" $ do

        it "success - parses '0 == 0' expression" $ do
            let operator = createToken EQUAL_EQUAL
                tokens = [createNumericToken 0, operator, createNumericToken 0]
            parse tokens `shouldParseTo` Binary (NumExpr 0) operator (NumExpr 0)

        it "success - parses '4 != 3' expression" $ do
            let operator = createToken BANG_EQUAL
                tokens = [createNumericToken 4, operator, createNumericToken 3]
            parse tokens `shouldParseTo` Binary (NumExpr 4) operator (NumExpr 3)

        it "success - parses '4 > 1 == 3 < 0' expression" $ do
            let tokens =
                    [ createNumericToken 4
                    , createToken GREATER
                    , createNumericToken 1
                    , createToken EQUAL_EQUAL
                    , createNumericToken 3
                    , createToken LESS
                    , createNumericToken 0
                    ]

                expectedExpression = Binary
                    (Binary (NumExpr 4) (createToken GREATER) (NumExpr 1))
                    (createToken EQUAL_EQUAL)
                    (Binary (NumExpr 3) (createToken LESS) (NumExpr 0))

            parse tokens `shouldParseTo` expectedExpression

    describe "invalid expressions" $ do

        it "fails - empty tokens input" $ do
            parse [] `shouldFailTo` ParseError "Expected expression" Nothing

        it "fails - incomplete expression" $ do
            let tokens = [createNumericToken 10, createToken STAR]
            parse tokens `shouldFailTo` ParseError "Expected expression" Nothing

        it "fails - invalid numeric token" $ do
            let badNumber = baseToken { tokenType = NUMBER, literal = StringValue "NaN"}
                tokens = [createNumericToken 10, createToken STAR, badNumber]
            parse tokens `shouldFailTo` ParseError "Failed to parse numeric literal" (Just badNumber)

        it "fails - invalid string token" $ do
            let badString = baseToken { tokenType = STRING, literal = NumberValue 10}
            parse [badString] `shouldFailTo` ParseError "Failed to parse string literal" (Just badString)

        it "fails - invalid literal" $ do
            let tok = createToken DOT
            parse [tok] `shouldFailTo` ParseError "Expected expression"(Just tok)
