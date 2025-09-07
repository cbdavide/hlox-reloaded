{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}

module Runtime (
    Interpreter,
    RuntimeInterrupt (..),
    RuntimeError (..),
    InterpreterContext (..),
    Callable (..),
    CallableImpl (..),
    Locals,
    -- Environment
    Environment,
    createEnv,
    envDefine,
    frameAssign,
    frameLookup,
    popFrame,
    pushFrame,
    globalEnvironment,
    -- Value
    Value (..),
    isNumber,
    isString,
    isTruthy,
    callableFromValue,
) where

import Control.Monad.Except (ExceptT)
import Control.Monad.Extra (ifM)
import Control.Monad.State (MonadIO (liftIO), StateT)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Token (Token)

type Locals = Map Token Int

data RuntimeInterrupt = ReturnValue Token Value | Error RuntimeError
    deriving (Eq, Show)

data RuntimeError = RuntimeError
    { token :: Token
    , errorMessage :: Text
    }
    deriving (Eq, Show)

data InterpreterContext = InterpreterContext
    { environment :: Environment
    , localsMap :: Locals
    }

data Value
    = NumberValue Float
    | StringValue Text
    | BooleanValue Bool
    | FunctionValue Callable
    | Nil
    deriving (Eq)

type Interpreter a = ExceptT RuntimeInterrupt (StateT InterpreterContext IO) a

data Callable = forall c. (CallableImpl c, Eq c) => Callable c

class CallableImpl c where
    arity :: c -> Int
    name :: c -> String
    call :: c -> [Value] -> Interpreter Value

instance Eq Callable where
    (==) :: Callable -> Callable -> Bool
    Callable c1 == Callable c2 = name c1 == name c2

instance CallableImpl Callable where
    arity :: Callable -> Int
    arity (Callable c) = arity c

    name :: Callable -> String
    name (Callable c) = name c

    call :: Callable -> [Value] -> Interpreter Value
    call (Callable c) = call c

type Frame = IORef (Map Text Value)
type Environment = [Frame]

createFrame :: IO Frame
createFrame = newIORef M.empty

frameLookup :: Text -> Frame -> IO (Maybe Value)
frameLookup k fr = readIORef fr >>= \m -> pure $ M.lookup k m

pushFrame :: Environment -> IO Environment
pushFrame env = createFrame >>= \fr -> pure (fr : env)

popFrame :: Environment -> Environment
popFrame = drop 1

frameIsMember :: Text -> Frame -> IO Bool
frameIsMember k fr = readIORef fr >>= \m -> pure $ M.member k m

frameInsert :: Text -> Value -> Frame -> IO ()
frameInsert k v fr = readIORef fr >>= \m -> writeIORef fr (M.insert k v m)

frameAssign :: Text -> Value -> Frame -> IO Bool
frameAssign k v fr = ifM (frameIsMember k fr) (frameInsert k v fr >> pure True) (pure False)

frameDefine :: Text -> Value -> Frame -> IO ()
frameDefine k v fr = readIORef fr >>= \m -> writeIORef fr (M.insert k v m)

createEnv :: IO Environment
createEnv = createFrame >>= \fr -> pure [fr]

envDefine :: Text -> Value -> Environment -> IO Bool
envDefine _ _ [] = pure False
envDefine k v (e : _) = frameDefine k v e >> pure True

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

callableFromValue :: Value -> Maybe Callable
callableFromValue (FunctionValue c) = Just c
callableFromValue _ = Nothing

instance Show Value where
    show :: Value -> String
    show Nil = "nil"
    show (StringValue v) = T.unpack v
    show (BooleanValue v) = show v
    show (FunctionValue v) = name v
    show (NumberValue v) = format v
      where
        format :: Float -> String
        format x
            | x == fromInteger (round x) = show (round x :: Int)
            | otherwise = show x

data NativeFunction = NativeFunction
    { fnName :: String
    , fnArity :: Int
    , fnBody :: [Value] -> Interpreter Value
    }

instance CallableImpl NativeFunction where
    name :: NativeFunction -> String
    name c = "<built-in function " <> fnName c <> ">"

    arity :: NativeFunction -> Int
    arity = fnArity

    call :: NativeFunction -> [Value] -> Interpreter Value
    call = fnBody

instance Eq NativeFunction where
    a == b = name a == name b

clockNativeFunction :: NativeFunction
clockNativeFunction = NativeFunction{fnName = "clock", fnArity = 0, fnBody = clockImpl}

clockImpl :: [Value] -> Interpreter Value
clockImpl _ = liftIO getPOSIXTime >>= \t -> pure $ NumberValue (realToFrac (t * 1000))

globalFunctions :: [NativeFunction]
globalFunctions = [clockNativeFunction]

globalEnvironment :: IO Environment
globalEnvironment = do
    newEnv <- createEnv
    mapM_ (\x -> envDefine (T.pack $ name x) (FunctionValue $ Callable x) newEnv) globalFunctions
    pure newEnv
