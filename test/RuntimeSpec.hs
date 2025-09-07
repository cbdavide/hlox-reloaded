{-# LANGUAGE OverloadedStrings #-}

module RuntimeSpec (runtimeSpecs) where

import Data.IORef (IORef, readIORef)
import Data.Map (Map)
import Runtime (Environment, Value (..), createEnv, envDefine, pushFrame)
import Test.Hspec (Spec, describe, it, shouldBe)

runtimeSpecs :: Spec
runtimeSpecs = describe "Runtime" $ do
    spec_Environment

spec_Environment :: Spec
spec_Environment = describe "environment" $ do
    it "environment test" $
        "hello" `shouldBe` "hello"

printFrame :: (Show v, Show k) => IORef (Map k v) -> IO ()
printFrame frame = do
    value <- readIORef frame
    putStrLn "\n\t===== Frame ===="
    print value

printEnvironment :: Environment -> IO ()
printEnvironment env = do
    putStrLn "**** ENV ****"
    mapM_ printFrame env
