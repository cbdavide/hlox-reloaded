{-# LANGUAGE  OverloadedStrings, PatternSynonyms #-}

module ParserSpec ( parserSpecs ) where

import Control.Monad ( forM_ )
import qualified Data.Text as T
import Literal ( Value (..) )
import Parser ( Expression (..), ParseError (..), Stmt(..), parseExpression, parseStmt )
import Test.Hspec ( describe, it, shouldBe, Spec, Expectation, expectationFailure )
import Token ( Token (..), TokenType (..) )
import Utils

baseToken :: Token
baseToken = Token 
    { tokenType=EOF
    , tokenLine=0
    , tokenLength=0
    , tokenColumn=0
    , lexeme=""
    , literal=Nil
    }

pattern BoolExpr :: Bool -> Expression
pattern BoolExpr b = Literal (BooleanValue b)

pattern NumExpr :: Float -> Expression
pattern NumExpr n = Literal (NumberValue n)

pattern StrExpr :: T.Text -> Expression
pattern StrExpr t = Literal (StringValue t)

createToken :: TokenType -> Token
createToken tp = baseToken { tokenType = tp }

semicolon :: Token
semicolon = createToken SEMICOLON

equal :: Token
equal = createToken EQUAL

createTokens :: [TokenType] -> [Token]
createTokens tps = createToken <$> tps

createNumericToken :: Float -> Token
createNumericToken num = (createToken NUMBER) { literal = NumberValue num }

createStringToken :: T.Text -> Token
createStringToken s = (createToken STRING) { literal = StringValue s }

shouldParseTo :: (Eq a, Show a, Show e) => Either e a -> a -> Expectation
shouldParseTo (Right actual) expected = actual `shouldBe` expected
shouldParseTo (Left err) _ = expectationFailure $ "Expected successful parse but got error: " ++ show err

shouldFailTo :: (Eq e, Show e, Show a) => Either e a -> e -> Expectation
shouldFailTo (Right a) _  = expectationFailure $ "Expected error but got successful response :" ++ show a
shouldFailTo (Left err) expected  =  err `shouldBe` expected

parserSpecs :: Spec
parserSpecs = describe "Parser" $ do
    spec_parseExpressions
    spec_parseStmts

spec_parseExpressions :: Spec
spec_parseExpressions = describe "parseExpression" $ do

    describe "primary rule" $ do
        
        let cases :: [(Token, Expression)]
            cases = [ (createToken TRUE, BoolExpr True)
                    , (createToken FALSE, BoolExpr False)
                    , (createToken NIL, Literal Nil)
                    , (createNumericToken 25, NumExpr 25)
                    , (createStringToken "hello", StrExpr "hello")
                    , (createToken IDENTIFIER, Variable $ createToken IDENTIFIER)
                    ]

        forM_ cases $ \(tp', expr') -> do
            it ("success - parses '" ++ show (tokenType tp') ++ "' literal") $ do
                parseExpression [tp'] `shouldParseTo` expr'

        it "success - parses grouped '(true)' expression" $ do
            let tokens = createTokens [LEFT_PAREN, TRUE, RIGHT_PAREN]
                expectedExpr = Grouping (Literal $ BooleanValue True)

            parseExpression tokens `shouldParseTo` expectedExpr

    describe "unary rule" $ do

        it "success - parses '!true' expression" $ do
            let operator = createToken BANG
                tokens = operator : createTokens [TRUE]
            parseExpression tokens `shouldParseTo` Unary operator (BoolExpr True)

        it "success - parses '-10' expression" $ do
            let operator = createToken MINUS
                tokens = [operator, createNumericToken 10]
            parseExpression tokens `shouldParseTo` Unary operator (NumExpr 10)

        it "success - parses nested '!-true' expression" $ do
            let operator1 = createToken BANG
                operator2 = createToken MINUS
                tokens = [operator1, operator2, createToken TRUE]
            parseExpression tokens `shouldParseTo` Unary operator1 (Unary operator2 (BoolExpr True))

    describe "factor rule" $ do

        it "success - parses '100 * 1' expression" $ do
            let operator = createToken STAR
                tokens = [createNumericToken 100, operator, createNumericToken 25]
            parseExpression tokens `shouldParseTo` Binary (NumExpr 100) operator (NumExpr 25)

        it "success - parses '2 / 0' expression" $ do
            let operator = createToken SLASH
                tokens = [createNumericToken 2, operator, createNumericToken 0]
            parseExpression tokens `shouldParseTo` Binary (NumExpr 2) operator (NumExpr 0)

        it "success - parses '(1 * 2) / 3' expression" $ do
            let operator1 = createToken STAR
                operator2 = createToken SLASH

                tokens = [ createNumericToken 1
                         , operator1
                         , createNumericToken 2
                         , operator2
                         , createNumericToken 3
                         ]

                expectedExpression = Binary
                    (Binary (NumExpr 1) operator1 (NumExpr 2))
                    operator2
                    (NumExpr 3)

            parseExpression tokens `shouldParseTo` expectedExpression

    describe "term rule" $ do

        it "success - parses '22 + 12' expression" $ do
            let operator = createToken PLUS
                tokens = [createNumericToken 22, operator, createNumericToken 12]
            parseExpression tokens `shouldParseTo` Binary (NumExpr 22) operator (NumExpr 12)

        it "success - parses '4 - 3' expression" $ do
            let operator = createToken SLASH
                tokens = [createNumericToken 4, operator, createNumericToken 3]
            parseExpression tokens `shouldParseTo` Binary (NumExpr 4) operator (NumExpr 3)

        it "success - parses '1 + (2 / 3) + 5' expression" $ do
            let sum' = createToken PLUS
                rest' = createToken MINUS
                div' = createToken SLASH

                tokens = [ createNumericToken 1
                         , sum'
                         , createNumericToken 2
                         , div'
                         , createNumericToken 3
                         , rest'
                         , createNumericToken 5
                         ]

                expectedExpression = Binary
                    (Binary (NumExpr 1) sum' (Binary (NumExpr 2) div' (NumExpr 3)))
                    rest'
                    (NumExpr 5)

            parseExpression tokens `shouldParseTo` expectedExpression

    describe "comparison rule" $ do
        let val1 = createNumericToken 10
            val2 = createNumericToken 15
            cases :: [(String, Token)]
            cases = [ (">", createToken GREATER)
                    , (">=", createToken GREATER_EQUAL)
                    , ("<", createToken LESS)
                    , ("<=", createToken LESS_EQUAL)
                    ]

        forM_ cases $ \(s', tp') -> do
            it ("success - parses '10 " ++ s' ++ " 15' expression") $ do
                parseExpression [val1, tp', val2] `shouldParseTo` Binary (NumExpr 10) tp' (NumExpr 15)

    describe "equality rule" $ do

        it "success - parses '0 == 0' expression" $ do
            let operator = createToken EQUAL_EQUAL
                tokens = [createNumericToken 0, operator, createNumericToken 0]
            parseExpression tokens `shouldParseTo` Binary (NumExpr 0) operator (NumExpr 0)

        it "success - parses '4 != 3' expression" $ do
            let operator = createToken BANG_EQUAL
                tokens = [createNumericToken 4, operator, createNumericToken 3]
            parseExpression tokens `shouldParseTo` Binary (NumExpr 4) operator (NumExpr 3)

        it "success - parses '4 > 1 == 3 < 0' expression" $ do
            let tokens =
                    [ createNumericToken 4
                    , createToken GREATER
                    , createNumericToken 1
                    , createToken EQUAL_EQUAL
                    , createNumericToken 3
                    , createToken LESS
                    , createNumericToken 0
                    ]

                expectedExpression = Binary
                    (Binary (NumExpr 4) (createToken GREATER) (NumExpr 1))
                    (createToken EQUAL_EQUAL)
                    (Binary (NumExpr 3) (createToken LESS) (NumExpr 0))

            parseExpression tokens `shouldParseTo` expectedExpression

    describe "assignment rule" $ do

        it "success - parses 'a = 10'" $ do
            let variable = createToken IDENTIFIER
                tokens = [variable, createToken EQUAL, createNumericToken 10]
                expectedExpression = Assign variable (NumExpr 10)

            parseExpression tokens `shouldParseTo` expectedExpression

        it "success - parses 'a = b = 16'" $ do
            let variable = createToken IDENTIFIER
                tokens = [variable, equal, variable, equal, createNumericToken 16]
                expectedExpression = Assign variable (Assign variable (NumExpr 16))

            parseExpression tokens `shouldParseTo` expectedExpression

        it "fails - invalid assignment target '2 = 16'" $ do
            let tokens = [createNumericToken 2, equal, createNumericToken 16]
                expectedError = ParseError "Invalid assignment target" (Just equal)
            parseExpression tokens `shouldFailTo` expectedError

    describe "logical rule" $ do

        it "success - parses '1 and true'" $ do
            let tokens = [ createNumericToken 1 , andToken , true ]
                expectedExpression = Logical (numExpr 1) andToken (boolExpr True)
            parseExpression tokens `shouldParseTo` expectedExpression

        it "success - parses 'false or \"hello\"" $ do
            let tokens = [ false , orToken , createStringToken "hello" ]
                expectedExpression = Logical (boolExpr False) orToken (strExpr "hello")
            parseExpression tokens `shouldParseTo` expectedExpression

        it "success - parses 'true or false and false or true'" $ do
            let tokens = [ true, orToken , false, andToken
                         , false, orToken, true ]
                expectedExpression =
                    Logical
                        (Logical (boolExpr True) orToken (Logical
                            (boolExpr False) andToken (boolExpr False)))
                        orToken
                        (boolExpr True)

            parseExpression tokens `shouldParseTo` expectedExpression

    describe "invalid expressions" $ do

        it "fails - empty tokens input" $ do
            parseExpression [] `shouldFailTo` ParseError "Expected expression" Nothing

        it "fails - incomplete expression" $ do
            let tokens = [createNumericToken 10, createToken STAR]
            parseExpression tokens `shouldFailTo` ParseError "Expected expression" Nothing

        it "fails - invalid numeric token" $ do
            let badNumber = baseToken { tokenType = NUMBER, literal = StringValue "NaN"}
                tokens = [createNumericToken 10, createToken STAR, badNumber]
            parseExpression tokens `shouldFailTo` ParseError "Failed to parse numeric literal" (Just badNumber)

        it "fails - invalid string token" $ do
            let badString = baseToken { tokenType = STRING, literal = NumberValue 10}
            parseExpression [badString] `shouldFailTo` ParseError "Failed to parse string literal" (Just badString)

        it "fails - invalid literal" $ do
            let tok = createToken DOT
            parseExpression [tok] `shouldFailTo` ParseError "Expected expression"(Just tok)

spec_parseStmts :: Spec
spec_parseStmts = describe "parseStmt" $ do

    describe "expression statment" $ do

        it "success - valid expression statment" $ do
            let tokens = [createStringToken "Hello", semicolon]
                expectedExpr = StrExpr "Hello"

            parseStmt tokens `shouldParseTo` Expression expectedExpr

        it "fails - invalid expression statment" $ do
            let tokens = [createStringToken "Hello"]
            parseStmt tokens `shouldFailTo` ParseError "Expected ';' after value" Nothing

    describe "print statment" $ do

        it "success - valid print statment" $ do
            let tokens = [createToken PRINT, createStringToken "Hello", semicolon]
                expectedExpr = StrExpr "Hello"

            parseStmt tokens `shouldParseTo` Print expectedExpr

        it "fails - invalid print statment" $ do
            let tokens = [createToken PRINT, createStringToken "Hello"]
            parseStmt tokens `shouldFailTo` ParseError "Expected ';' after value" Nothing

    describe "block statement" $ do

        it "success - valid statement block" $ do
            let tokens =
                    [ createToken LEFT_BRACE
                    , createToken PRINT, createStringToken "Hello, ", semicolon
                    , createToken PRINT, createStringToken "World!", semicolon
                    , createToken RIGHT_BRACE
                    ]

                expectedStmt = Block [Print (StrExpr "Hello, "), Print (StrExpr "World!")]

            parseStmt tokens `shouldParseTo` expectedStmt

        it "fails - statement block with missing '}'" $ do
            let tokens =
                    [ createToken LEFT_BRACE
                    , createToken PRINT, createStringToken "Hello, ", semicolon
                    , createToken PRINT, createStringToken "World!", semicolon
                    -- mising '}'
                    ]

            parseStmt tokens `shouldFailTo` ParseError "Expected '}' after a block" Nothing

    describe "var statment" $ do

        it "success - assignment statement with value" $ do
            let identifier = createToken IDENTIFIER
                tokens =
                    [ createToken VAR
                    , identifier
                    , createToken EQUAL
                    , createNumericToken 10
                    , semicolon
                    ]

            parseStmt tokens `shouldParseTo` Var identifier (NumExpr 10)

        it "success - assignment statement without value" $ do
            let identifier = createToken IDENTIFIER
                tokens = [ createToken VAR , identifier , semicolon ]

            parseStmt tokens `shouldParseTo` Var identifier (Literal Nil)

        it "fails - assignment statement without identifier" $ do
            let strToken = createStringToken "hello"
                tokens = [ createToken VAR , strToken, semicolon ]

            parseStmt tokens `shouldFailTo` ParseError "Expected variable name" (Just strToken)

        it "fails - invalid assignment statement" $ do
            let identifier = createToken IDENTIFIER
                tokens = [ createToken VAR , identifier ]

            parseStmt tokens `shouldFailTo` ParseError "Expected ';' after variable declaration" Nothing

    describe "if statement" $ do

        it "success" $ do
            let tokens =
                    [ -- if (true)
                      createToken IF, createToken LEFT_PAREN, true, createToken RIGHT_PAREN
                      -- 10;
                    , createNumericToken 10, semicolon
                    ]
                expectedStmt = IfStmt (boolExpr True) (Expression $ numExpr 10) Nothing
            parseStmt tokens `shouldParseTo` expectedStmt

        it "success - with else branch" $ do
            let tokens =
                    [ -- if (false)
                      createToken IF, createToken LEFT_PAREN, false, createToken RIGHT_PAREN
                      -- 10;
                    , createNumericToken 10, semicolon
                      -- else 15;
                    , createToken ELSE, createNumericToken 15, semicolon
                    ]
                expectedStmt = IfStmt (boolExpr False)
                                      (Expression $ numExpr 10)
                                      (Just . Expression $ numExpr 15)

            parseStmt tokens `shouldParseTo` expectedStmt

        it "fails - missing expected '(' " $ do
            let leftBrace = createToken LEFT_BRACE
                tokens =  [ createToken IF, leftBrace ]
            parseStmt tokens `shouldFailTo` ParseError "Expected '(' after 'if'" (Just leftBrace)

        it "fails - missing expected ')' " $ do
            let tokens =  [ createToken IF, createToken LEFT_PAREN, true ]
            parseStmt tokens `shouldFailTo` ParseError "Expected ')' after 'if'" Nothing

    describe "while statement" $ do

        it "success" $ do
            let tokens =
                    [ -- while (false)
                      createToken WHILE, createToken LEFT_PAREN, false, createToken RIGHT_PAREN
                      -- "Hello";
                    , createStringToken "Hello", semicolon
                    ]
                expectedStmt = WhileStmt (boolExpr False) (Expression $ strExpr "Hello")
            parseStmt tokens `shouldParseTo` expectedStmt

        it "fails - missing expected '('" $ do
            let leftBrace = createToken LEFT_BRACE
                tokens =  [ createToken WHILE, createToken LEFT_BRACE ]
            parseStmt tokens `shouldFailTo` ParseError "Expected '(' after 'while'" (Just leftBrace)

        it "fails - missing expected ')'" $ do
            let tokens =  [ createToken WHILE, createToken LEFT_PAREN, true ]
            parseStmt tokens `shouldFailTo` ParseError "Expected ')' after 'while'" Nothing

    describe "for statement" $ do

        let varInititalizer = [ createToken VAR, createToken IDENTIFIER, equal, createNumericToken 0 ]
            exprInitializer = [ createToken IDENTIFIER, equal, createNumericToken 1 ]
            condition = [ createToken IDENTIFIER, createToken LESS_EQUAL, createNumericToken 10 ]
            increment = [ createToken IDENTIFIER, equal, createToken IDENTIFIER, plus, createNumericToken 1 ]
            body = [ createNumericToken 21, semicolon ]

            varInitializerStmt = Var (createToken IDENTIFIER) (numExpr 0)

            exprInitializerStmt = Expression $ Assign (createToken IDENTIFIER) (numExpr 1)

            conditionExpr = Binary (Variable (createToken IDENTIFIER))
                                   (createToken LESS_EQUAL)
                                   (numExpr 10)

            incrementStmt = Expression $ Assign (createToken IDENTIFIER)
                   (Binary (Variable (createToken IDENTIFIER)) (createToken PLUS) (numExpr 1))

            bodyExpr = Expression (numExpr 21)

        it "success" $ do
            let tokens = [ createToken FOR, createToken LEFT_PAREN ] ++ varInititalizer ++
                         [ semicolon ] ++ condition ++ [ semicolon ] ++ increment ++
                         [ createToken RIGHT_PAREN ] ++ body

                expectedStmt = Block
                        [ varInitializerStmt
                        , WhileStmt conditionExpr (Block [bodyExpr, incrementStmt])
                        ]

            parseStmt tokens `shouldParseTo` expectedStmt

        it "success - with initializer expression" $ do
            let tokens = [ createToken FOR, createToken LEFT_PAREN ] ++ exprInitializer ++
                         [ semicolon ] ++ condition ++ [ semicolon ] ++ increment ++
                         [ createToken RIGHT_PAREN ] ++ body

                expectedStmt = Block
                        [ exprInitializerStmt
                        , WhileStmt conditionExpr (Block [bodyExpr, incrementStmt])
                        ]

            parseStmt tokens `shouldParseTo` expectedStmt

        it "success - no initializer" $ do
            let tokens = [ createToken FOR, createToken LEFT_PAREN, semicolon ] ++
                         condition ++ [ semicolon ] ++ increment ++ [ createToken RIGHT_PAREN ] ++
                         body

                expectedStmt = WhileStmt conditionExpr (Block [ bodyExpr, incrementStmt ])

            parseStmt tokens `shouldParseTo` expectedStmt

        it "success - no condition" $ do
            let tokens = [ createToken FOR, createToken LEFT_PAREN ] ++ varInititalizer ++
                         [ semicolon, semicolon ] ++ increment ++ [ createToken RIGHT_PAREN ] ++
                         body

                expectedStmt = Block
                        [ varInitializerStmt
                        , WhileStmt (boolExpr True) (Block [ bodyExpr, incrementStmt ])
                        ]

            parseStmt tokens `shouldParseTo` expectedStmt

        it "success - no increment" $ do
            let tokens = [ createToken FOR, createToken LEFT_PAREN ] ++ varInititalizer ++
                         [ semicolon ] ++ condition ++ [ semicolon, createToken RIGHT_PAREN ] ++
                         body

                expectedStmt = Block
                        [ varInitializerStmt
                        , WhileStmt conditionExpr bodyExpr
                        ]

            parseStmt tokens `shouldParseTo` expectedStmt

        it "fails - missing '('" $ do
            let tokens = [ createToken FOR ] ++ varInititalizer ++
                         [ semicolon ] ++ condition ++ [ semicolon, createToken RIGHT_PAREN ] ++
                         body

            parseStmt tokens `shouldFailTo` ParseError "Expected '(' after 'for'" (Just $ createToken VAR)

        it "fails - missing ')'" $ do
            let tokens = [ createToken FOR, createToken LEFT_PAREN ] ++ varInititalizer ++
                         [ semicolon ] ++ condition ++ [ semicolon ] ++
                         body


            parseStmt tokens `shouldFailTo` ParseError "Expected ')' after for clauses" (Just semicolon)

        it "fails - missing ';' after initializer" $ do
            let invalidInitializer = [ createToken IDENTIFIER, equal, createNumericToken 10 ]
                tokens = [ createToken FOR, createToken LEFT_PAREN ] ++ invalidInitializer ++
                         condition ++ [ semicolon ] ++
                         body


            parseStmt tokens `shouldFailTo` ParseError "Expected ';' after value" (Just (createToken IDENTIFIER))

        it "fails - missing ';' after condition" $ do
            let tokens = [ createToken FOR, createToken LEFT_PAREN ] ++ varInititalizer ++
                         [ semicolon ] ++ condition ++ [ createToken RIGHT_PAREN ] ++
                         body


            parseStmt tokens `shouldFailTo` ParseError
                    "Expected ';' after loop condition"
                    (Just (createToken RIGHT_PAREN))
