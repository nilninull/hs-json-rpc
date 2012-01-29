{-# LANGUAGE OverloadedStrings #-}

module Network.JsonRpc.Common (
      user_agent,
      JsonRpcRequest,
      JsonRpcResponse,
      mkJsonRpcRequest,
      JsonRpcMessage,
      getAnswer,
      getId
) where

import Data.Maybe
import Data.Aeson
import Data.Aeson.Types
import Data.Functor
import Control.Applicative
import Control.Monad

user_agent :: String
user_agent = "Haskell JSON-RPC Client/0.0"

default_version :: String
default_version = "2.0"

data JsonRpcError = JsonRpcError Int String

instance FromJSON JsonRpcError where
  parseJSON (Object o) = JsonRpcError <$> o .: "code" <*> o .: "message"
  parseJSON _ = mzero

data JsonRpcRequest = Request Value String [Value]

instance ToJSON JsonRpcRequest where
  toJSON (Request iden meth params) =
    let mandatory = ["jsonrpc" .= default_version, "id" .= iden, "method" .= meth]
    in if (null params)
          then object mandatory
          else object (mandatory ++ ["params" .= params])

data JsonRpcResponse = Response Value (Either JsonRpcError Value)

instance FromJSON JsonRpcResponse where
  parseJSON (Object o) = do
    v <- (o .: "jsonrpc") :: Parser String
    if (v == "2.0") then
        do
          i <- o .: "id"
          temp_a <- (o .:? "result")
          a <- maybe (liftM Left (o .: "error")) (return . Right) temp_a
          return (Response i a)
        else
          mzero
  parseJSON _ = mzero
--  parseJSON _ = Response Null (Left (JsonRpc2Response -32603 "Internal error"))

class JsonRpcMessage a where
  getId :: a -> Value

instance JsonRpcMessage JsonRpcRequest where
  getId (Request iden _ _) = iden

instance JsonRpcMessage JsonRpcResponse where
  getId (Response iden _) = iden

mkJsonRpcRequest :: String -> [Value] -> JsonRpcRequest
mkJsonRpcRequest meth params = Request "hs-json-rpc" meth params

getAnswer :: JsonRpcResponse -> Either JsonRpcError Value
getAnswer (Response _ resp) = resp