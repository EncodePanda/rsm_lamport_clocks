module Main where

import Data.Functor
import Control.Monad
import Data.ConfigFile
import Data.List.NonEmpty
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Node

data AppError = AppCPError CPError
              | AppNodeError NodeError
                deriving Show

main :: IO ()
main = do
  nodesErr <- runExceptT $ readNodes "app.conf"
  either (putStrLn . show) (void . traverse (putStrLn . show)) nodesErr

readNodes :: String -> ExceptT AppError IO (NonEmpty Node)
readNodes configFile =  do
  conf <- withExceptT AppCPError $ join $ lift $ readfile emptyCP configFile
  hosts <- withExceptT AppCPError $ get conf "DEFAULT" "hosts"
  withExceptT AppNodeError $ ExceptT $ return $ parseNodeAddresses hosts
