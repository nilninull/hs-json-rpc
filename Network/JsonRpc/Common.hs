{-# LANGUAGE OverloadedStrings #-}

module Network.JsonRpc.Common (
      user_agent,
      Version2Request,
      Version2Response,
      Version1Request,
      Version1Response,
      JsonRpcMessage(..),
      JsonRpcRequest(..),
      JsonRpcResponse(..)
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

class JsonRpcMessage a where
  getId :: a -> Value

class (JsonRpcMessage a) => JsonRpcResponse a where
  getReturnValue :: a -> Either JsonRpcError Value

class (JsonRpcMessage a) => JsonRpcRequest a where
  mkJsonRpcRequest :: String -> [Value] -> a

data JsonRpcError = JsonRpcError Int String

instance FromJSON JsonRpcError where
  parseJSON (Object o) = JsonRpcError <$> o .: "code" <*> o .: "message"
  parseJSON _ = mzero

data Version2Request = Request Value String [Value]

instance ToJSON Version2Request where
  toJSON (Request iden meth params) =
    let mandatory = ["jsonrpc" .= default_version, "id" .= iden, "method" .= meth]
    in if (null params)
          then object mandatory
          else object (mandatory ++ ["params" .= params])

instance JsonRpcMessage Version2Request where
  getId (Request iden _ _) = iden

instance JsonRpcRequest Version2Request where
  mkJsonRpcRequest meth params = Request "hs-json-rpc" meth params

data Version2Response = Response Value (Either JsonRpcError Value)

instance FromJSON Version2Response where
  parseJSON (Object o) = do
    v <- (o .: "jsonrpc") :: Parser String
    if (v == "2.0") then
        do
          i <- o .: "id"
          temp_a <- (o .:? "result")
          a <- maybe (liftM Left (o .: "error")) (return . Right) temp_a
          return (Response i a)
        else
          return (Response Null (Left (JsonRpcError (-32600) "Invalid JSON-RPC")))
  parseJSON _ = return (Response Null (Left (JsonRpcError (-32600) "Invalid JSON-RPC")))

instance JsonRpcMessage Version2Response where
  getId (Response iden _) = iden

instance JsonRpcResponse Version2Response where
  getReturnValue (Response _ resp) = resp

data Version1Request = Request1 Value String [Value]

instance ToJSON Version1Request where
  toJSON (Request1 iden meth params) = object ["id" .= iden, "method" .= meth, "params" .= params]

instance JsonRpcMessage Version1Request where
  getId (Request1 iden _ _) = iden

instance JsonRpcRequest Version1Request where
  mkJsonRpcRequest meth params = Request1 "hs-json-rpc" meth params

data Version1Response = Response1 Value (Either JsonRpcError Value)

instance FromJSON Version1Response where
  parseJSON (Object o) = do
   i <- o .: "id"
   a <- o .: "result"
   if (a == Null)
     then
       do
         e <- o .: "error"
         return (Response1 i (Left e))
     else return (Response1 i (Right a))
  parseJSON _ = return (Response1 Null (Left (JsonRpcError (-32600) "Invalid JSON-RPC")))

instance JsonRpcMessage Version1Response where
  getId (Response1 iden _) = iden

instance JsonRpcResponse Version1Response where
  getReturnValue (Response1 _ resp) = resp