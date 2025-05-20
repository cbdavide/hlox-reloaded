{-# LANGUAGE  OverloadedStrings #-}
module InterpreterSpec ( interpreterSpecs ) where

import Control.Monad ( forM_ )
import Test.Hspec (Spec, describe, it, shouldBe)
import Interpreter (interpretExpression)
import Literal (LiteralValue (..))
import Parser (Expression (Literal))

interpreterSpecs :: Spec
interpreterSpecs = describe "Interpreter" $ do
    spec_eval_expression

spec_eval_expression :: Spec
spec_eval_expression = describe "evalExpression" $ do

    describe "literal expression" $ do
        let cases :: [(String, LiteralValue)]
            cases = [ ("number", NumberValue 10)
                    , ("string", StringValue "hello")
                    , ("boolean", BooleanValue True)
                    , ("nil", Nil)
                    ]

        forM_ cases $ \(name', val') -> do
            it ("success - evals " ++ name' ++ " literal") $ do
                result <- interpretExpression (Literal val')
                result `shouldBe` Right val'

