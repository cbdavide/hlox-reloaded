{-# LANGUAGE  OverloadedStrings #-}

module ScannerSpec (scannerSpecs) where

import Control.Monad
import Control.Monad.State
import Data.Either
import qualified Data.Text as T
import Test.Hspec

import Scanner
import qualified Test.Tasty.Runners as T

scannerSpecs :: Spec
scannerSpecs = describe "Scanner" $ do
    spec_advance
    spec_advanceIfMatches
    spec_scanTokens

baseScannerCtx :: ScannerContext
baseScannerCtx = ScannerContext 
    { tokens=[]
    , source=""
    , line=0
    , column=0
    , errors=[]
    , currentLexeme=""
    }

getTokens :: ScannerResult -> [Token]
getTokens = fromRight []

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

spec_scanTokens :: Spec
spec_scanTokens = describe "scanTokens" $ do

    describe "scan operators" $ do

        let cases :: [(T.Text, TokenType)]
            cases =
                [ ("(", LEFT_PAREN), (")", RIGHT_PAREN) , ("{", LEFT_BRACE)
                , ("}", RIGHT_BRACE), (",", COMMA) , (".", DOT), ("-", MINUS)
                , ("+", PLUS), (";", SEMICOLON) , ("/", SLASH), ("*", STAR)

                , ("!", BANG), ("!=", BANG_EQUAL) , ("=", EQUAL), ("==", EQUAL_EQUAL)
                , (">", GREATER), (">=", GREATER_EQUAL) , ("<", LESS), ("<=", LESS_EQUAL)
                ]

        forM_ cases $ \(s', tp') -> do
            it ("success - scans " ++ T.unpack s' ++ " operator") $ do
                let result = scanTokens s'
                    tokens' = getTokens result

                isRight result `shouldBe` True
                length tokens' `shouldBe` 2
                (tokenType . head) tokens' `shouldBe` tp'
                (lexeme . head) tokens' `shouldBe` s'

    describe "scan ignores blank characters" $ do

        let cases :: [T.Text]
            cases = ["\n", " ", "\t", "\r"]

        let format :: T.Text -> String
            format t = T.unpack $ case t of
                "\n" -> "\\n"
                "\t" -> "\\t"
                "\r" -> "\\r"
                x -> x

        forM_ cases $ \s' -> do
            it ("success - ignores '" ++ format s' ++ "'") $ do
                let result = scanTokens s'
                    tokens' = getTokens result

                isRight result `shouldBe` True
                length tokens' `shouldBe` 1
                (tokenType . head) tokens' `shouldBe` EOF
                (lexeme . head) tokens' `shouldBe` ""

    it "success - ignores comments" $ do
        let result = scanTokens "// this is a comment"

        isRight result `shouldBe` True
        (length . getTokens) result `shouldBe` 1
        (tokenType . head . getTokens) result `shouldBe` EOF

    it "success - scan tokens after comment with line break" $ do
        let result = scanTokens "// this is a comment\n!"

        isRight result `shouldBe` True
        (length . getTokens) result `shouldBe` 2
        (tokenType . head . getTokens) result `shouldBe` BANG

