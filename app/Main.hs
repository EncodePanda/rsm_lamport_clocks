module Main where

import Data.ConfigFile
import Control.Monad.Except
import Control.Monad.Trans.Except
import Node

main :: IO ()
main = do
  Right nodes <- runExceptT $ readNodes "app.conf"
  putStrLn (show nodes)

readNodes :: String -> ExceptT CPError IO [Node]
readNodes configFile =  do
  conf <- join $ liftIO $ readfile emptyCP configFile
  hosts <- get conf "DEFAULT" "hosts"
  return $ parseNodeAddresses hosts
