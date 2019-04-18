module Main where

import Data.ConfigFile
import Control.Monad.Except
import Control.Monad.Trans.Except
import Node

main :: IO ()
main = do
          Right nodes <- runExceptT $
              do
              conf <- join $ liftIO $ readfile emptyCP "app.conf"
              hosts <- get conf "DEFAULT" "hosts"
              return $ parseNodeAddresses hosts
          putStrLn (show nodes)
