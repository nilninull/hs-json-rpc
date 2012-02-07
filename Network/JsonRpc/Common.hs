{-|
This module contains utility functions and types for the library.

Client code shouldn't use this module: client-side applications should use "Network.JsonRpc.Client"
-}
module Network.JsonRpc.Common (
      user_agent,
      Version2Request,
      Version2Response,
      Version2Notice,
      Version1Request,
      Version1Response,
      Version1Notice,
      JsonRpcException(..),
      JsonRpcMessage(..),
      JsonRpcRequest(..),
      JsonRpcNotice(..),
      JsonRpcResponse(..)
) where

import qualified Data.Text as T
import Data.Maybe
import Data.Aeson
import Data.Aeson.Types
import Data.Functor
import Data.Typeable
import Control.Applicative
import Control.Exception
import Control.Monad

user_agent :: String
user_agent = "Haskell JSON-RPC/0.0"

-- | A typeclass representing JSON-RPC messages: requests, response and notifications.
class JsonRpcMessage a where
  -- | A function to get the id of a JSON-RPC message. By convention, notifications have a null id. 
  getId :: a -> Value

-- | A typeclass representing server responses
class (JsonRpcMessage a) => JsonRpcResponse a where
  getReturnValue :: a -> Either JsonRpcException Value

-- | A typeclass representing client requests
class (JsonRpcMessage a) => JsonRpcRequest a where
  mkJsonRpcRequest :: String -> [Value] -> [Pair] -> a

-- | A typeclass representing client notifications
class (JsonRpcMessage a) => JsonRpcNotice a where
  mkJsonRpcNotice :: String -> [Value] -> [Pair] -> a

-- | The exception type returned by "Network.JsonRpc.Client"'s functions when an error appens 
data JsonRpcException = JsonRpcException Int String (Maybe Value) deriving (Show)

instance Typeable JsonRpcException where
  typeOf _ =
    let con = mkTyCon "Network.JsonRpc.Common.JsonRpcException" -- mkTyCon3 "hs-json-rpc" "Network.JsonRpc.Common" "JsonRpcException"
    in mkTyConApp con []

instance Exception JsonRpcException

instance FromJSON JsonRpcException where
  parseJSON (Object o) = JsonRpcException <$> o .: T.pack "code" <*> o .: T.pack "message" <*> o .:? T.pack "data"
  parseJSON _ = return (JsonRpcException (-32600) "Invalid JSON-RPC" Nothing)

data Version2Request = Request Value String [Value] [Pair]

instance ToJSON Version2Request where
  toJSON (Request iden meth params custom_elems) =
    let version = "2.0"
        mandatory = [T.pack "jsonrpc" .= version, T.pack "id" .= iden, T.pack "method" .= meth]
        parameters = if null params
                        then []
                        else [T.pack "params" .= params]
        full_list = mandatory ++ parameters ++ custom_elems
    in object full_list

instance JsonRpcMessage Version2Request where
  getId (Request iden _ _ _) = iden

instance JsonRpcRequest Version2Request where
  mkJsonRpcRequest = Request (toJSON "hs-json-rpc")

data Version2Notice = Notice String [Value] [Pair]

instance ToJSON Version2Notice where
  toJSON (Notice meth params custom_elems) =
    let version = "2.0"
        mandatory = [T.pack "jsonrpc" .= version, T.pack "method" .= meth]
        parameters = if null params
                        then []
                        else [T.pack "params" .= params]
        full_list = mandatory ++ parameters ++ custom_elems
    in object full_list

instance JsonRpcMessage Version2Notice where
  getId _ = Null

instance JsonRpcNotice Version2Notice where
  mkJsonRpcNotice = Notice

data Version2Response = Response Value (Either JsonRpcException Value)

instance FromJSON Version2Response where
  parseJSON (Object o) = do
    v <- (o .: T.pack "jsonrpc") :: Parser String
    if v == "2.0" then
        do
          i <- o .: T.pack "id"
          temp_a <- o .:? T.pack "result"
          a <- maybe (liftM Left (o .: T.pack "error")) (return . Right) temp_a
          return (Response i a)
        else
          return (Response Null (Left (JsonRpcException (-32600) "Invalid JSON-RPC" Nothing)))
  parseJSON _ = return (Response Null (Left (JsonRpcException (-32600) "Invalid JSON-RPC" Nothing)))

instance JsonRpcMessage Version2Response where
  getId (Response iden _) = iden

instance JsonRpcResponse Version2Response where
  getReturnValue (Response _ resp) = resp

data Version1Request = Request1 Value String [Value] [Pair]

instance ToJSON Version1Request where
  toJSON (Request1 iden meth params custom_elems) = object ([T.pack "id" .= iden, T.pack "method" .= meth, T.pack "params" .= params] ++ custom_elems)

instance JsonRpcMessage Version1Request where
  getId (Request1 iden _ _ _) = iden

instance JsonRpcRequest Version1Request where
  mkJsonRpcRequest = Request1 (toJSON "hs-json-rpc")

data Version1Notice = Notice1 String [Value] [Pair]

instance ToJSON Version1Notice where
  toJSON (Notice1 meth params custom_elems) = object ([T.pack "id" .= Null, T.pack "method" .= meth, T.pack "params" .= params] ++ custom_elems)

instance JsonRpcMessage Version1Notice where
  getId _ = Null

instance JsonRpcNotice Version1Notice where
  mkJsonRpcNotice = Notice1

data Version1Response = Response1 Value (Either JsonRpcException Value)

instance FromJSON Version1Response where
  parseJSON (Object o) = do
   i <- o .: T.pack "id"
   a <- o .: T.pack "result"
   if a == Null
     then
       do
         e <- o .: T.pack "error"
         return (Response1 i (Left e))
     else return (Response1 i (Right a))
  parseJSON _ = return (Response1 Null (Left (JsonRpcException (-32600) "Invalid JSON-RPC" Nothing)))

instance JsonRpcMessage Version1Response where
  getId (Response1 iden _) = iden

instance JsonRpcResponse Version1Response where
  getReturnValue (Response1 _ resp) = resp