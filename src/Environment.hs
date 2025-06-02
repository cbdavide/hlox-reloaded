module Environment (
  Environment
, createEnv
, envAssign
, envDefine
, envLookup
, popFrame
, pushFrame
) where

import Control.Applicative ((<|>))
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as M
import Literal ( LiteralValue )

type Frame = Map Text LiteralValue
type Environment = [Frame]

createEnv :: Environment
createEnv = [M.empty]

pushFrame :: Environment -> Environment
pushFrame env = M.empty : env

popFrame :: Environment -> Environment
popFrame = drop 1

envLookup :: Text -> Environment -> Maybe LiteralValue
envLookup k = foldr ((<|>) . M.lookup k) Nothing

envAssign :: Text -> LiteralValue -> Environment -> Maybe Environment
envAssign k v env = envLookup k env >> Just (envAssign' k v env)

envDefine :: Text -> LiteralValue -> Environment -> Maybe Environment
envDefine _ _ [] = Nothing
envDefine k v (e:es) = Just $ frameDefine k v e : es

envAssign' :: Text -> LiteralValue -> Environment -> Environment
envAssign' _ _ [] = []
envAssign' k v (e:es) = case frameAssign k v e of
    Nothing -> e : envAssign' k v es
    Just new -> new : es

frameAssign :: Text -> LiteralValue -> Frame -> Maybe Frame
frameAssign k v fr = if M.member k fr then Just $ M.insert k v fr else Nothing

frameDefine :: Text -> LiteralValue -> Frame -> Frame
frameDefine = M.insert

