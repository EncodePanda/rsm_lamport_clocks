module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Node


data Arg = Arg String String String
           deriving Show

main :: IO ()
main = send =<< execParser opts
  where
    opts = info (parseArgs <**> helper)
      ( fullDesc
     <> progDesc "Client sending commands to the node"
     <> header "rsm-client" )

    send = putStrLn . show

parseArgs :: Parser Arg
parseArgs = Arg
      <$> strOption
          ( long "serverAddr"
         <> metavar "HOST:PORT"
         <> help "represents server address to which client will send a command" )
      <*> strOption
          ( long "clientAddr"
         <> metavar "HOST:PORT"
         <> help "represents client address on which it listents to responses" )
      <*> strOption
          ( long "command"
         <> metavar "COMMAND"
         <> help "string representation of the command being send to the server" )          
