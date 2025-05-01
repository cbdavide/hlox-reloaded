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
    deriving (Eq, Show)

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
