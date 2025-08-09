{-# LANGUAGE OverloadedStrings #-}

module RuntimeSpec (runtimeSpecs) where

import Data.IORef (IORef, readIORef)
import Data.Map (Map)
import Runtime (Environment, Value (..), createEnv, envAssign, envDefine, envLookup, pushFrame)
import Test.Hspec (Spec, describe, it, shouldBe)

runtimeSpecs :: Spec
runtimeSpecs = describe "Runtime" $ do
    spec_Environment

spec_Environment :: Spec
spec_Environment = describe "parseExpression" $ do
    describe "envAssign" $ do
        it "success - assigns to defined variable" $ do
            env <- createEnv

            _ <- envDefine "hello" (NumberValue 10) env

            result <- envAssign "hello" (NumberValue 21) env

            result `shouldBe` True
            value <- envLookup "hello" env
            value `shouldBe` Just (NumberValue 21)

        it "success - assigns to variable on different frame" $ do
            env <- createEnv >>= pushFrame

            _ <- envDefine "you" (StringValue "tube") env

            newEnv <- pushFrame env
            result <- envAssign "you" (NumberValue 100) newEnv

            result `shouldBe` True
            value <- envLookup "you" newEnv
            value `shouldBe` Just (NumberValue 100)

        it "fails - assigns to a not defined variable" $ do
            env <- createEnv >>= pushFrame
            _ <- envDefine "you" (StringValue "tube") env
            resultEnv <- envAssign "tube" (NumberValue 100) env
            resultEnv `shouldBe` False

printFrame :: (Show v, Show k) => IORef (Map k v) -> IO ()
printFrame frame = do
    value <- readIORef frame
    putStrLn "\n\t===== Frame ===="
    print value

printEnvironment :: Environment -> IO ()
printEnvironment env = do
    putStrLn "**** ENV ****"
    mapM_ printFrame env
