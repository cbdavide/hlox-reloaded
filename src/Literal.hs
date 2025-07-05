{-# LANGUAGE InstanceSigs #-}
module Literal (
  Value (..)
, isNumber
, isString
, isTruthy
) where

import qualified Data.Text as T

data Value =
      NumberValue Float
    | StringValue T.Text
    | BooleanValue Bool
    | Nil
    deriving (Eq)

isTruthy :: Value -> Bool
isTruthy Nil = False
isTruthy (BooleanValue b) = b
isTruthy _ = True

isNumber :: Value -> Bool
isNumber (NumberValue _) = True
isNumber _ = False

isString :: Value -> Bool
isString (StringValue _) = True
isString _ = False

instance Show Value where

    show :: Value -> String
    show Nil = "nil"
    show (StringValue v) = T.unpack v
    show (BooleanValue v) = show v
    show (NumberValue v) = format v
        where format :: Float -> String
              format x
                | x == fromInteger (round x) = show (round x :: Int)
                | otherwise = show x
