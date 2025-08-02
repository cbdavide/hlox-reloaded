{-# LANGUAGE  OverloadedStrings #-}
module InterpreterSpec ( interpreterSpecs ) where

import Control.Monad ( forM_ )
import Test.Hspec (Spec, describe, it, shouldBe, Expectation, expectationFailure)
import Interpreter (interpretExpression )
import Parser (Expression (..))
import Runtime ( Value(..), RuntimeError(..) )
import Utils
    ( bang, boolExpr, minus, nilExpr, numExpr, plus, strExpr )

shouldEvalTo :: (Eq a, Show a, Show e) => Either e a -> a -> Expectation
shouldEvalTo (Right actual) expected = actual `shouldBe` expected
shouldEvalTo (Left err) _ = expectationFailure $ "Expected successful eval but got error: " ++ show err

shouldFailTo :: (Eq e, Show e, Show a) => Either e a -> e -> Expectation
shouldFailTo (Right a) _  = expectationFailure $ "Expected error but got successful response :" ++ show a
shouldFailTo (Left err) expected  =  err `shouldBe` expected

interpreterSpecs :: Spec
interpreterSpecs = describe "Interpreter" $ do
    spec_eval_expression

spec_eval_expression :: Spec
spec_eval_expression = describe "evalExpression" $ do

    describe "literal expression" $ do
        let cases :: [(String, Value)]
            cases = [ ("number", NumberValue 10)
                    , ("string", StringValue "hello")
                    , ("boolean", BooleanValue True)
                    , ("nil", Nil)
                    ]

        forM_ cases $ \(name', val') -> do
            it ("success - evals " ++ name' ++ " literal") $ do
                result <- interpretExpression (Literal val')
                result `shouldEvalTo` val'

    describe "unary expression" $ do

        let cases :: [(String, Expression, Value)]
            cases = [ ("-10", Unary minus (numExpr 10), NumberValue (-10))
                    , ("- -1", Unary minus (Unary minus (numExpr 1)), NumberValue 1)
                    , ("!true", Unary bang (boolExpr True), BooleanValue False)
                    , ("!false", Unary bang (boolExpr False), BooleanValue True)
                    , ("!21", Unary bang (numExpr 21), BooleanValue False)
                    , ("!\"hello\"", Unary bang (strExpr "hello"), BooleanValue False)
                    , ("!nil", Unary bang nilExpr, BooleanValue True)
                    ]

        forM_ cases $ \(name', expr', val') -> do
            it ("success - evals '" ++ name' ++ "'") $ do
                result <- interpretExpression expr'
                result `shouldEvalTo` val'

        it "fails - invalid expression unexpected operator" $ do
            let expr = Unary plus (numExpr 16)

            result <- interpretExpression expr
            result `shouldFailTo` RuntimeError plus "unary expression with unexpected operator"

        it "fails - invalid expression '-\"bye\"'" $ do
            let expr = Unary minus (strExpr "bye")

            result <- interpretExpression expr
            result `shouldFailTo` RuntimeError minus "operand must be a number"
