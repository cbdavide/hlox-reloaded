{-# LANGUAGE  OverloadedStrings #-}

module ParserSpec ( parserSpecs ) where

import Control.Monad ( forM_ )
import Test.Hspec ( describe, it, shouldBe, Spec, Expectation, expectationFailure )

import Scanner ( Token (..), TokenType (..), Value (..) )
import Parser ( Expression (..), ParseError (..), LiteralValue(..), parse )

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

shouldParseTo :: (Eq a, Show a, Show e) => Either e a -> a -> Expectation
shouldParseTo (Right actual) expected = actual `shouldBe` expected
shouldParseTo (Left err) _ = expectationFailure $ "Expected successful parse but got error: " ++ show err

parserSpecs :: Spec
parserSpecs = describe "Parser" $ do
    spec_parse

spec_parse :: Spec
spec_parse = describe "parse" $ do

    describe "unary expressions" $ do
        
        let cases :: [(TokenType, Expression)]
            cases = [ (TRUE, Literal (BooleanValue True))
                    , (FALSE, Literal (BooleanValue False))
                    , (NIL, Literal Nil)
                    ]

        forM_ cases $ \(tp', expr') -> do
            it ("success - parses '" ++ show tp' ++ "' token") $ do
                parse [createToken tp'] `shouldParseTo` expr'
