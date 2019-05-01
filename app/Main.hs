module Main where

import Data.ConfigFile
import Data.List.NonEmpty
import Control.Monad.Except
import Control.Monad.Trans.Except
import Node

data AppError = AppCPError CPError
              | AppNodeError NodeError
                deriving Show

main :: IO ()
main = do
  nodesErr <- runExceptT $ readNodes "app.conf"
  either (putStrLn . show) (putStrLn . show) nodesErr

readNodes :: String -> ExceptT AppError IO (NonEmpty Node)
readNodes configFile =  do
  conf <- withExceptT AppCPError $ join $ liftIO $ readfile emptyCP configFile
  hosts <- withExceptT AppCPError $ get conf "DEFAULT" "hosts"
  withExceptT AppNodeError $ except $ parseNodeAddresses hosts
