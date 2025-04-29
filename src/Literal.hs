module Literal (
  LiteralValue (..)
) where

import qualified Data.Text as T

data LiteralValue =
      NumberValue Float
    | StringValue T.Text
    | BooleanValue Bool
    | Nil
    deriving (Eq, Show)
