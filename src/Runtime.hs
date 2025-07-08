{-# LANGUAGE ExistentialQuantification, InstanceSigs  #-}

module Runtime (
  Interpreter
, RuntimeError (..)
, InterpreterContext (..)

  -- Environment
 ,Environment
, createEnv
, envAssign
, envDefine
, envLookup
, popFrame
, pushFrame

  -- Value
, Value (..)
, isNumber
, isString
, isTruthy
) where

import Control.Applicative ((<|>))
import Control.Monad.Except ( ExceptT )
import Control.Monad.State ( StateT )
import Data.Map (Map)
import Data.Text (Text)
import Token (Token)
import qualified Data.Map as M
import qualified Data.Text as T

data RuntimeError = RuntimeError
    { token         :: Token
    , errorMessage  :: Text
    } deriving (Eq, Show)

newtype InterpreterContext = InterpreterContext
    { environment :: Environment
    } deriving (Eq, Show)

data Value =
      NumberValue Float
    | StringValue Text
    | BooleanValue Bool
    | FunctionValue Callable
    | Nil
    deriving (Eq)

type Interpreter a = ExceptT RuntimeError (StateT InterpreterContext IO) a

data Callable = forall c. (CallableImpl c, Eq c) => Callable c

class CallableImpl c where
    name :: c -> String
    arity :: c -> Int

instance Eq Callable where
    (==) :: Callable -> Callable -> Bool
    Callable c1 == Callable c2 = name c1 == name c2

type Frame = Map Text Value
type Environment = [Frame]

createEnv :: Environment
createEnv = [M.empty]

pushFrame :: Environment -> Environment
pushFrame env = M.empty : env

popFrame :: Environment -> Environment
popFrame = drop 1

envLookup :: Text -> Environment -> Maybe Value
envLookup k = foldr ((<|>) . M.lookup k) Nothing

envAssign :: Text -> Value -> Environment -> Maybe Environment
envAssign k v env = envLookup k env >> Just (envAssign' k v env)

envDefine :: Text -> Value -> Environment -> Maybe Environment
envDefine _ _ [] = Nothing
envDefine k v (e:es) = Just $ frameDefine k v e : es

envAssign' :: Text -> Value -> Environment -> Environment
envAssign' _ _ [] = []
envAssign' k v (e:es) = case frameAssign k v e of
    Nothing -> e : envAssign' k v es
    Just new -> new : es

frameAssign :: Text -> Value -> Frame -> Maybe Frame
frameAssign k v fr = if M.member k fr then Just $ M.insert k v fr else Nothing

frameDefine :: Text -> Value -> Frame -> Frame
frameDefine = M.insert

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
    show (FunctionValue _) = "<fn: function>"
    show (NumberValue v) = format v
        where format :: Float -> String
              format x
                | x == fromInteger (round x) = show (round x :: Int)
                | otherwise = show x

