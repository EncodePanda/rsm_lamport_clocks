module Main where

import Data.ConfigFile
import Control.Monad.Error
import Lib

main :: IO ()
main = do
          rv <- runErrorT $
              do
              conf <- join $ liftIO $ readfile emptyCP "app.conf"
              hosts <- get conf "DEFAULT" "hosts"
              liftIO $ putStrLn $ "Known hosts: " ++ hosts
              --return ()
          print rv
