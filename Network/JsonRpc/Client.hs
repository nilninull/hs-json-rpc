{-# LANGUAGE FlexibleInstances #-}

module Network.JsonRpc.Client (
      remote,
      remoteWithVersion,
--      notify,
--      notifyWithVersion,
      JsonRpcCall,
      JsonRpcVersion(..),
--      JsonRpcNotice
) where

import Network.JsonRpc.Common
import Data.Aeson
import Network.HTTP
import Network.Stream as NS
import Network.URI
import Control.Exception
import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Lazy as BS

data JsonRpcVersion = Version1 | Version2

remote :: (ToJSON a, JsonRpcCall b) => String -> String -> (a -> b)
remote = remoteWithVersion Version2

remoteWithVersion :: (ToJSON a, JsonRpcCall b) => JsonRpcVersion -> String -> String -> (a -> b)
remoteWithVersion version url nom = (\x -> marshalAndCall version url nom (\xs -> (toJSON x):xs))

class JsonRpcCall a where
  marshalAndCall :: JsonRpcVersion -> String -> String -> ([Value] -> [Value]) -> a

instance (ToJSON a, JsonRpcCall b) => JsonRpcCall (a -> b) where
  marshalAndCall version url nom f = (\x -> marshalAndCall version url nom (\xs -> f ((toJSON x):xs)))

instance (FromJSON a) => JsonRpcCall (IO a) where
  marshalAndCall version url nom f =
    case version of
         Version1 -> callv1 url (mkJsonRpcRequest nom (f []))
         Version2 -> call url (mkJsonRpcRequest nom (f []))

call :: (FromJSON a) => String -> Version2Request -> IO a
call url request =
  let req_id = getId request
      payload = encode request
  in do
       res <- sendPost url payload
       response <- getResponseBody res
       processResponse response req_id True

callv1 :: (FromJSON a) => String -> Version1Request -> IO a
callv1 url request =
  let req_id = getId request
      payload = encode request
  in do
       res <- sendPost url payload
       response <- getResponseBody res
       processResponse1 response req_id False

sendPost :: String -> BS.ByteString -> IO (NS.Result (Response BS.ByteString))
sendPost url payload =
  let uri = fromMaybe (error "Invalid URI") (parseURI url)
      headers = [Header HdrUserAgent user_agent, Header HdrContentType "application/json", Header HdrContentLength (show (BS.length payload))]
      req = Request uri POST headers payload
  in simpleHTTP req

processResponse :: (FromJSON a) => BS.ByteString -> Value -> Bool ->  IO a
processResponse resp req_id is_v2 =
  let response = fromMaybe (error "Invalid server response") (decode' resp) :: Version2Response
      resp_id = getId response
      resp_id_err = is_v2 && resp_id == Null
  in if (resp_id == req_id || resp_id_err) then
        do
          case (getReturnValue response) of
              Left _ -> error "JSON-RPC error"
              Right val -> case (fromJSON val) of
                                Error _ -> error "Type mismatch"
                                Success a -> return a
        else
          error "Bad Id"

processResponse1 :: (FromJSON a) => BS.ByteString -> Value -> Bool ->  IO a
processResponse1 resp req_id is_v2 =
  let response = fromMaybe (error "Invalid server response") (decode' resp) :: Version1Response
      resp_id = getId response
      resp_id_err = is_v2 && resp_id == Null
  in if (resp_id == req_id || resp_id_err) then
        do
          case (getReturnValue response) of
              Left _ -> error "JSON-RPC error"
              Right val -> case (fromJSON val) of
                                Error _ -> error "Type mismatch"
                                Success a -> return a
        else
          error "Bad Id"