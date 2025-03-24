{-# LANGUAGE  OverloadedStrings #-}

module ScannerSpec (scannerSpecs) where

import Test.Hspec
import Control.Monad.State

import Scanner ( ScannerContext (..), advance, advanceIfMatches )

scannerSpecs :: Spec
scannerSpecs = describe "Scanner" $ do
    spec_advance
    spec_advanceIfMatches

baseScannerCtx :: ScannerContext
baseScannerCtx = ScannerContext 
    { tokens=[]
    , source=""
    , line=0
    , errors=[]
    , currentLexeme=""
    }

spec_advance :: Spec
spec_advance = describe "advance" $ do

    it "fails - if the source is empty" $ do
        let ctx = baseScannerCtx { source = "" }
            result = evalState advance ctx
        
        result `shouldBe` Nothing

    it "success - consumes the next character" $ do
        let ctx = baseScannerCtx { source = "hello", currentLexeme = "hig", line = 0 }
            (result, newState) = runState advance ctx
        
        result `shouldBe` Just 'h'
        currentLexeme newState `shouldBe` "high"
        source newState `shouldBe` "ello"
        line newState `shouldBe` 0


spec_advanceIfMatches :: Spec
spec_advanceIfMatches = describe "advanceIfMatches" $ do
    
    it "fails - if the source is empty" $ do
        let ctx = baseScannerCtx { source = "" }
            result = evalState (advanceIfMatches (== 'a')) ctx
        
        result `shouldBe` False

    it "fails - if source next char doesn't match" $ do
        let ctx = baseScannerCtx { source = "c" }
            result = evalState (advanceIfMatches (== 'a')) ctx
        
        result `shouldBe` False

    it "success - if source next char matches" $ do
        let ctx = baseScannerCtx { source = "c", currentLexeme = "" }
            (result, newState) = runState (advanceIfMatches (== 'c')) ctx
        
        result `shouldBe` True
        source newState `shouldBe` ""
        currentLexeme newState `shouldBe` "c"

