module Main where

import Network.JsonRpc.Client

test :: String -> IO String
test = detailledRemote Version1 [] "http://jsonrpcphp.org/server.php" "giveMeSomeData"

main = do
  n <- test "name"
  a <- test "attr"
  putStrLn ("Name: " ++ n ++ " - Attr: " ++ a)
