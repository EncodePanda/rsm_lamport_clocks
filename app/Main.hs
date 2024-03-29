module Main where

import Data.ConfigFile
import Data.List.NonEmpty
import Control.Monad.Except
import Control.Monad.Trans.Except
import Node

data AppError = AppCPError CPError | AppNodeError NodeError

main :: IO ()
main = do
  Right nodes <- runExceptT $ readNodes "app.conf"
  putStrLn (show nodes)

readNodes :: String -> ExceptT AppError IO (NonEmpty Node)
readNodes configFile =  do
  conf <- withExceptT AppCPError $ join $ liftIO $ readfile emptyCP configFile
  hosts <- withExceptT AppCPError $ get conf "DEFAULT" "hosts"
  withExceptT AppNodeError $ except $ parseNodeAddresses hosts
