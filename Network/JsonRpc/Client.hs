{-# LANGUAGE FlexibleInstances #-}

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

data JsonRpcVersion = Version1 | Version2

remote :: JsonRpcCall c => String -> String -> c
remote = detailledRemote Version2 []

notify :: JsonRpcNotification n => String -> String -> n
notify = detailledNotify Version2 []

detailledRemote :: JsonRpcCall c => JsonRpcVersion -> [Pair] -> String -> String -> c
detailledRemote version custom_elems url nom = marshalAndCall version custom_elems url nom id

detailledNotify :: JsonRpcNotification n => JsonRpcVersion -> [Pair] -> String -> String -> n
detailledNotify version custom_elems url nom = marshalAndNotify version custom_elems url nom id

class JsonRpcCall a where
  marshalAndCall :: JsonRpcVersion -> [Pair] -> String -> String -> ([Value] -> [Value]) -> a

instance (ToJSON a, JsonRpcCall b) => JsonRpcCall (a -> b) where
  marshalAndCall version custom_elems url nom f = (\x -> marshalAndCall version custom_elems url nom (\xs -> f ((toJSON x):xs)))

instance (FromJSON a) => JsonRpcCall (IO a) where
  marshalAndCall version custom_elems url nom f =
    case version of
         Version1 -> callv1 url (mkJsonRpcRequest nom (f []) custom_elems)
         Version2 -> call url (mkJsonRpcRequest nom (f []) custom_elems)

class JsonRpcNotification a where
  marshalAndNotify :: JsonRpcVersion -> [Pair] -> String -> String -> ([Value] -> [Value]) -> a

instance (ToJSON a, JsonRpcNotification b) => JsonRpcNotification (a -> b) where
  marshalAndNotify version custom_elems url nom f = (\x -> marshalAndNotify version custom_elems url nom (\xs -> f ((toJSON x):xs)))

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
  in do
       _ <- sendPost url payload
       return ()

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
  in do
       _ <- sendPost url payload
       return ()

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
  in if (resp_id == req_id || resp_id == Null) then
        do
          case (getReturnValue response) of
              Left err -> throwIO err
              Right val -> case (fromJSON val) of
                                Error _ -> throwIO (JsonRpcException (-32009) "Type mismatch" Nothing)
                                Success a -> return a
        else
          throwIO (JsonRpcException (-32039) "Invalid response id" Nothing)

processResponsev1 :: (FromJSON a) => BS.ByteString -> Value -> IO a
processResponsev1 resp req_id =
  let response = fromMaybe (throw (JsonRpcException (-32019) "Invalid server response" Nothing)) (decode' resp) :: Version1Response
      resp_id = getId response
  in if (resp_id == req_id) then
        do
          case (getReturnValue response) of
              Left err -> throwIO err
              Right val -> case (fromJSON val) of
                                Error _ -> throwIO (JsonRpcException (-32009) "Type mismatch" Nothing)
                                Success a -> return a
        else
          throwIO (JsonRpcException (-32039) "Invalid response id" Nothing)