module Main where

import Data.ConfigFile
import Control.Monad.Except
import Control.Monad.Trans.Except
import Node

data AppError = AppCPError CPError | AppNodeError NodeError

main :: IO ()
main = do
  Right nodes <- runExceptT $ readNodes "app.conf"
  putStrLn (show nodes)

readNodes :: String -> ExceptT AppError IO [Node]
readNodes configFile =  do
  conf <- withExceptT AppCPError $ join $ liftIO $ readfile emptyCP configFile
  hosts <- withExceptT AppCPError $ get conf "DEFAULT" "hosts"
  return $ parseNodeAddresses hosts
