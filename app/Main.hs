module Main where

import Data.ConfigFile
import Control.Monad.Except
import Control.Monad.Trans.Except
import Lib

main :: IO ()
main = do
          rv <- runExceptT $
              do
              conf <- join $ liftIO $ readfile emptyCP "app.conf"
              hosts <- get conf "DEFAULT" "hosts"
              liftIO $ putStrLn $ "Known hosts: " ++ hosts
              --return ()
          print rv
