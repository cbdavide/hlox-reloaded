{-# LANGUAGE  OverloadedStrings #-}

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

createToken :: TokenType -> Token
createToken tp = baseToken { tokenType = tp }

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

    describe "unary expressions" $ do
        
        let cases :: [(Token, Expression)]
            cases = [ (createToken TRUE, Literal (BooleanValue True))
                    , (createToken FALSE, Literal (BooleanValue False))
                    , (createToken NIL, Literal Nil)
                    , (createNumericToken 25, Literal (Parser.NumberValue 25))
                    , (createStringToken "hello", Literal (Parser.StringValue "hello"))
                    ]

        forM_ cases $ \(tp', expr') -> do
            it ("success - parses '" ++ show (tokenType tp') ++ "' token") $ do
                parse [tp'] `shouldParseTo` expr'
