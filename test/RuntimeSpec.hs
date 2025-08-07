{-# LANGUAGE OverloadedStrings #-}

module RuntimeSpec (runtimeSpecs) where

import Data.Maybe (fromJust, isJust)
import Runtime (Value (NumberValue, StringValue), createEnv, envAssign, envDefine, envLookup, pushFrame)
import Test.Hspec (Spec, describe, it, shouldBe)

runtimeSpecs :: Spec
runtimeSpecs = describe "Runtime" $ do
    spec_Environment

spec_Environment :: Spec
spec_Environment = describe "parseExpression" $ do
    describe "envAssign" $ do
        it "success - assigns to defined variable" $ do
            let env = envDefine "hello" (NumberValue 10) createEnv
                resultEnv = envAssign "hello" (NumberValue 21) (fromJust env)

            isJust resultEnv `shouldBe` True
            envLookup "hello" (fromJust resultEnv) `shouldBe` Just (NumberValue 21)

        it "success - assigns to variable on different frame" $ do
            let env = (pushFrame . pushFrame) (fromJust $ envDefine "you" (StringValue "tube") (pushFrame createEnv))
                resultEnv = envAssign "you" (NumberValue 100) env

            isJust resultEnv `shouldBe` True
            envLookup "you" (fromJust resultEnv) `shouldBe` Just (NumberValue 100)

        it "fails - assigns to a not defined variable" $ do
            let env = fromJust $ envDefine "you" (StringValue "tube") createEnv
                resultEnv = envAssign "tube" (NumberValue 100) env

            resultEnv `shouldBe` Nothing

