{-# LANGUAGE  OverloadedStrings #-}

module ScannerSpec (scannerSpecs) where

import Control.Monad ( forM_ )
import Data.Either ( fromLeft, fromRight, isLeft, isRight )
import Data.List.NonEmpty (fromList, NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as T
import Test.Hspec ( describe, it, shouldBe, Spec )

import Scanner
    ( scanTokens
    , Error(..)
    , ScannerResult
    , Token(..)
    , TokenType(..)
    , Value(..)
    )

scannerSpecs :: Spec
scannerSpecs = describe "Scanner" $ do
    spec_scanTokens

getTokens :: ScannerResult -> NonEmpty Token
getTokens a = fromList $ fromRight [] a

getErrors :: ScannerResult -> NonEmpty Error
getErrors a = fromList $ fromLeft [] a

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
                (tokenType . NonEmpty.head) tokens' `shouldBe` tp'
                (lexeme . NonEmpty.head) tokens' `shouldBe` s'

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
                (tokenType . NonEmpty.head) tokens' `shouldBe` EOF
                (lexeme . NonEmpty.head) tokens' `shouldBe` ""

    it "success - ignores comments" $ do
        let result = scanTokens "// this is a comment"

        isRight result `shouldBe` True
        (length . getTokens) result `shouldBe` 1
        (tokenType . NonEmpty.head . getTokens) result `shouldBe` EOF

    it "success - scans tokens after comment with line break" $ do
        let result = scanTokens "// this is a comment\n!"

        isRight result `shouldBe` True
        (length . getTokens) result `shouldBe` 2
        (tokenType . NonEmpty.head . getTokens) result `shouldBe` BANG

    it "success - scans multiple tokens" $ do
        let result = scanTokens "(==)"
            tokens' = getTokens result

        isRight result `shouldBe` True
        (length . getTokens) result `shouldBe` 4

        NonEmpty.head tokens' `shouldBe` Token
            { tokenType=LEFT_PAREN
            , tokenLine=1
            , tokenLength=1
            , tokenColumn=1
            , literal= StringValue "tmp"
            , lexeme="("
            }

        (tokens' NonEmpty.!! 1) `shouldBe` Token
            { tokenType=EQUAL_EQUAL
            , tokenLine=1
            , tokenLength=2
            , tokenColumn=2
            , literal= StringValue "tmp"
            , lexeme="=="
            }

        (tokens' NonEmpty.!! 2) `shouldBe` Token
            { tokenType=RIGHT_PAREN
            , tokenLine=1
            , tokenLength=1
            , tokenColumn=4
            , literal= StringValue "tmp"
            , lexeme=")"
            }

        tokenType (tokens' NonEmpty.!! 2) `shouldBe` RIGHT_PAREN

    it "success - scans string" $ do
        let result = scanTokens "\"hello i'm a string\""
            tokens' = getTokens result

            expectedLexeme = "hello i'm a string"

        isRight result `shouldBe` True
        length tokens' `shouldBe` 2
        (tokenType . NonEmpty.head) tokens' `shouldBe` STRING
        (lexeme . NonEmpty.head) tokens' `shouldBe` expectedLexeme

    it "fails - scanning invalid string" $ do
        let input =  "\"hello i'm an invalid string\n"
            expectedLexeme = T.dropEnd 1 input -- \n is not scanned
            result = scanTokens input
            errors' = getErrors result

        isLeft result `shouldBe` True
        length errors' `shouldBe` 1
        (errorMessage . NonEmpty.head) errors' `shouldBe` "unterminated string"
        (errorLexeme . NonEmpty.head) errors' `shouldBe` expectedLexeme
        (errorLexemeLength . NonEmpty.head) errors' `shouldBe` T.length expectedLexeme
        (errorColumn . NonEmpty.head) errors' `shouldBe` 1

    it "fails - scanning invalid string EOF" $ do
        let input =  "\"hello i'm an invalid string"
            result = scanTokens input
            errors' = getErrors result

        isLeft result `shouldBe` True
        length errors' `shouldBe` 1
        (errorMessage . NonEmpty.head) errors' `shouldBe` "unterminated string"
        (errorLexeme . NonEmpty.head) errors' `shouldBe` input
        (errorColumn . NonEmpty.head) errors' `shouldBe` 1
