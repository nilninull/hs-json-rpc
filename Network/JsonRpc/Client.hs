{-# LANGUAGE FlexibleInstances #-}

{-|
This module contains the client functionality of JSON-RPC, whose specification may be read here (<http://json-rpc.org/wiki/specification>)
for version 1 and here (<http://www.jsonrpc.org/spec.html>) for version 2.

By convention, a notification is represented Haskell-side by a function returning a IO (), given it doesn't care about the server's
response.

Note that the functions returned by 'remote', 'notify', 'detailledRemote' and 'detailledNotify' need not have parameters.
-}
module Network.JsonRpc.Client (
      remote,
      detailledRemote,
      notify,
      detailledNotify,
      JsonRpcCall,
      JsonRpcVersion(..),
      JsonRpcNotification,
      JsonRpcException
) where

import Network.JsonRpc.Common
import Data.Aeson
import Data.Aeson.Types
import Network.HTTP
import Network.Stream as NS
import Network.URI
import Control.Exception
import Data.Maybe
import qualified Data.ByteString.Lazy as BS

-- | Datatype representing the protocol's version in use during a call or notification.
data JsonRpcVersion = Version1 | Version2

-- | Call a remote function via JSON-RPC, using version 2 of the protocol and HTTP POST as its transport
remote :: JsonRpcCall c
          => String -- ^ Server URI. May contain username and password on the format username:password\@ before the hostname.
          -> String -- ^ Method name.
          -> c      -- ^ A function (ToJSON x1...ToJSON xn, FromJSON y) => x1 -> ... -> xn -> IO y
remote = detailledRemote Version2 []

-- | Call a remote notification on a JSON-RPC server, using version 2 of the protocol and HTTP POST as its transport
notify :: JsonRpcNotification n
          => String -- ^ Server URI. May contain username and password on the format username:password\@ before the hostname.
          -> String -- ^ Notification name.
          -> n      -- ^ A function (ToJSON x1...ToJSON xn) => x1 -> ... -> xn -> IO ()
notify = detailledNotify Version2 []

-- | Call a remote function via JSON-RPC, specifying the protocol's version to use and customs elements to add to the request
detailledRemote :: JsonRpcCall c
                   => JsonRpcVersion -- ^ Protocol's version.
                   -> [Pair]         -- ^ Custom JSON elements to add to the request
                   -> String         -- ^ Server URI. May contain username and password on the format username:password\@ before the hostname.
                   -> String         -- ^ Method name.
                   -> c              -- ^ A function (ToJSON x1...ToJSON xn, FromJSON y) => x1 -> ... -> xn -> IO y
detailledRemote version custom_elems url nom = marshalAndCall version custom_elems url nom id

-- | Call a remote notification on a JSON-RPC server, specifying the protocol's version to use and customs elements to add to the request
detailledNotify :: JsonRpcNotification n
                   => JsonRpcVersion -- ^ Protocol's version.
                   -> [Pair]         -- ^ Custom JSON elements to add to the request
                   -> String         -- ^ Server URI. May contain username and password on the format username:password\@ before the hostname.
                   -> String         -- ^ Notification name.
                   -> n              -- ^ A function (ToJSON x1...ToJSON xn, FromJSON y) => x1 -> ... -> xn -> IO y
detailledNotify version custom_elems url nom = marshalAndNotify version custom_elems url nom id

-- | Type of functions representable in JSON-RPC
class JsonRpcCall a where
  marshalAndCall :: JsonRpcVersion -> [Pair] -> String -> String -> ([Value] -> [Value]) -> a

instance (ToJSON a, JsonRpcCall b) => JsonRpcCall (a -> b) where
  marshalAndCall version custom_elems url nom f x = marshalAndCall version custom_elems url nom (\xs -> f ((toJSON x):xs))

instance (FromJSON a) => JsonRpcCall (IO a) where
  marshalAndCall version custom_elems url nom f =
    case version of
         Version1 -> callv1 url (mkJsonRpcRequest nom (f []) custom_elems)
         Version2 -> call url (mkJsonRpcRequest nom (f []) custom_elems)

-- | Type of notifications representable in JSON-RPC
class JsonRpcNotification a where
  marshalAndNotify :: JsonRpcVersion -> [Pair] -> String -> String -> ([Value] -> [Value]) -> a

instance (ToJSON a, JsonRpcNotification b) => JsonRpcNotification (a -> b) where
  marshalAndNotify version custom_elems url nom f x = marshalAndNotify version custom_elems url nom (\xs -> f ((toJSON x):xs))

instance JsonRpcNotification (IO ()) where
  marshalAndNotify version custom_elems url nom f =
    case version of
         Version1 -> notificationv1 url (mkJsonRpcNotice nom (f []) custom_elems)
         Version2 -> notification url (mkJsonRpcNotice nom (f []) custom_elems)

call :: (FromJSON a) => String -> Version2Request -> IO a
call url request =
  let req_id = getId request
      payload = encode request
  in do
       res <- sendPost url payload
       response <- getResponseBody res
       processResponse response req_id

notification :: String -> Version2Notice -> IO ()
notification url request =
  let req_id = getId request
      payload = encode request
  in sendPost url payload >> return ()

callv1 :: (FromJSON a) => String -> Version1Request -> IO a
callv1 url request =
  let req_id = getId request
      payload = encode request
  in do
       res <- sendPost url payload
       response <- getResponseBody res
       processResponsev1 response req_id

notificationv1 :: String -> Version1Notice -> IO ()
notificationv1 url request =
  let req_id = getId request
      payload = encode request
  in sendPost url payload >> return ()

sendPost :: String -> BS.ByteString -> IO (NS.Result (Response BS.ByteString))
sendPost url payload =
  let uri = fromMaybe (throw (JsonRpcException (-32029) "Invalid URI" Nothing)) (parseURI url)
      headers = [Header HdrUserAgent user_agent, Header HdrContentType "application/json", Header HdrContentLength (show (BS.length payload))]
      req = Request uri POST headers payload
  in simpleHTTP req

processResponse :: (FromJSON a) => BS.ByteString -> Value -> IO a
processResponse resp req_id =
  let response = fromMaybe (throw (JsonRpcException (-32019) "Invalid server response" Nothing)) (decode' resp) :: Version2Response
      resp_id = getId response
  in if resp_id `elem` [req_id, Null] then
        case getReturnValue response of
            Left err -> throwIO err
            Right val -> case fromJSON val of
                              Error _ -> throwIO (JsonRpcException (-32009) "Type mismatch" Nothing)
                              Success a -> return a
        else
          throwIO (JsonRpcException (-32039) "Invalid response id" Nothing)

processResponsev1 :: (FromJSON a) => BS.ByteString -> Value -> IO a
processResponsev1 resp req_id =
  let response = fromMaybe (throw (JsonRpcException (-32019) "Invalid server response" Nothing)) (decode' resp) :: Version1Response
      resp_id = getId response
  in if resp_id == req_id then
        case getReturnValue response of
            Left err -> throwIO err
            Right val -> case fromJSON val of
                              Error _ -> throwIO (JsonRpcException (-32009) "Type mismatch" Nothing)
                              Success a -> return a
        else
          throwIO (JsonRpcException (-32039) "Invalid response id" Nothing)