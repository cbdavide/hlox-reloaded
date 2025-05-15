{-# LANGUAGE InstanceSigs #-}
module Literal (
  LiteralValue (..)
, isNumber
, isString
, isTruthy
) where

import qualified Data.Text as T

data LiteralValue =
      NumberValue Float
    | StringValue T.Text
    | BooleanValue Bool
    | Nil
    deriving (Eq)

isTruthy :: LiteralValue -> Bool
isTruthy Nil = False
isTruthy (BooleanValue b) = b
isTruthy _ = True

isNumber :: LiteralValue -> Bool
isNumber (NumberValue _) = True
isNumber _ = False

isString :: LiteralValue -> Bool
isString (StringValue _) = True
isString _ = False

instance Show LiteralValue where

    show :: LiteralValue -> String
    show Nil = "nil"
    show (StringValue v) = T.unpack v
    show (BooleanValue v) = show v
    show (NumberValue v) = format v
        where format :: Float -> String
              format x
                | x == fromInteger (round x) = show (round x :: Int)
                | otherwise = show x
