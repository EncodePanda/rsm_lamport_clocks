module Node where

import Data.List.Split

data NodeError = NodeAddressNotParsable String

type Host = String
type Port = Int
data Node = Node Host Port
  deriving Show

parseNodeAddresses :: String -> Either NodeError [Node]
parseNodeAddresses =  traverse (toNode . (splitOn ":")) . (splitOn ",")
  where
    toNode (host:port:[]) = Right $ Node host (read port)
    toNode line = Left $ NodeAddressNotParsable $ show line

