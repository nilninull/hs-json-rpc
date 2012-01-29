{-# LANGUAGE FlexibleInstances #-}

module Network.JsonRpc.Client (
      remote,
      remoteWithVersion,
--      notify,
--      notifyWithVersion,
      JsonRpcCall,
--      JsonRpcNotice
) where

import Network.JsonRpc.Common
import Data.Aeson
import Network.HTTP
import Network.URI
import Control.Exception
import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Lazy as BS

remote :: (ToJSON a, JsonRpcCall b) => String -> String -> (a -> b)
remote = remoteWithVersion

remoteWithVersion :: (ToJSON a, JsonRpcCall b) => String -> String -> (a -> b)
remoteWithVersion url nom = (\x -> marshalAndCall url nom (\xs -> (toJSON x):xs))

class JsonRpcCall a where
  marshalAndCall :: String -> String -> ([Value] -> [Value]) -> a

instance (ToJSON a, JsonRpcCall b) => JsonRpcCall (a -> b) where
  marshalAndCall url nom f = (\x -> marshalAndCall url nom (\xs -> f ((toJSON x):xs)))

instance JsonRpcCall (IO String) where
  marshalAndCall url nom f = call url (mkJsonRpcRequest nom (f []))

call :: String -> JsonRpcRequest -> IO String
call url request = 
  let uri = fromMaybe (error "Invalid URI") (parseURI url)
      js = encode request
      headers = [Header HdrUserAgent user_agent, Header HdrContentType "application/json", Header HdrContentLength (show (BS.length js))]
      req = Request uri POST headers js
      req_id = getId request
  in do
      res <- simpleHTTP req
      body <- getResponseBody res
      return (show body)
      {- let response = fromMaybe (error "Invalid server response") (decode' body)
      if (getId response == req_id) then
        do
          case (getAnswer response) of
              Left _ -> error "JSON-RPC error"
              Right val -> case (fromJSON val) of
                                Error _ -> error "Type mismatch"
                                Success a -> return a
        else
        error "Bad Id" -}